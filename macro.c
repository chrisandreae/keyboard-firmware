/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  Licensed under the GNU GPL v2 (see GPL2.txt).

  See Kinesis.h for keyboard hardware documentation.

  ==========================

  If built for V-USB, this program includes library and sample code from:
	 V-USB, (C) Objective Development Software GmbH
	 Licensed under the GNU GPL v2 (see GPL2.txt)

  ==========================

  If built for LUFA, this program includes library and sample code from:
			 LUFA Library
	 Copyright (C) Dean Camera, 2011.

  dean [at] fourwalledcubicle [dot] com
		   www.lufa-lib.org

  Copyright 2011  Dean Camera (dean [at] fourwalledcubicle [dot] com)

  Permission to use, copy, modify, distribute, and sell this
  software and its documentation for any purpose is hereby granted
  without fee, provided that the above copyright notice appear in
  all copies and that both that the copyright notice and this
  permission notice and warranty disclaimer appear in supporting
  documentation, and that the name of the author not be used in
  advertising or publicity pertaining to distribution of the
  software without specific, written prior permission.

  The author disclaim all warranties with regard to this
  software, including all implied warranties of merchantability
  and fitness.  In no event shall the author be liable for any
  special, indirect or consequential damages or any damages
  whatsoever resulting from loss of use, data or profits, whether
  in an action of contract, negligence or other tortious action,
  arising out of or in connection with the use or performance of
  this software.
*/

#include "hardware.h"

#include "macro.h"

#include "usb.h"
#include "printing.h"
#include "serial_eeprom.h"
#include "buzzer.h"

#include <stdint.h>
#include <stdlib.h>
#include <avr/eeprom.h>
#include <util/delay.h>

// The macro lookup index is in internal eeprom
#define MACRO_INDEX_COUNT (MACRO_INDEX_SIZE / sizeof(macro_idx))
static macro_idx macro_index[MACRO_INDEX_COUNT] EEMEM;

// and the macro data itself is in external eeprom
static uint8_t macros_storage[MACROS_SIZE] EEEXT;
static uint16_t *const macros_end_offset = (uint16_t*)macros_storage;
static uint8_t  *const macros = macros_storage + sizeof(uint16_t);


////////////////////// Macro Execution ////////////////////////
static macro_data* current_macro;
static hid_keycode* current_macro_cursor;
static macro_idx* current_macro_index;

////////////////////// Macro Management ////////////////////////

uint8_t* macros_get_storage(){
	return &macros_storage[0];
}
uint8_t* macros_get_index(){
	return (uint8_t*) &macro_index[0];
}

void macros_reset_defaults(){
	macro_idx tmp;
	memset(tmp.keys, NO_KEY, MACRO_MAX_KEYS);
	tmp.macro_offset = 0x0;

	for(uint8_t i = 0; i < MACRO_INDEX_COUNT; ++i){
		eeprom_update_block(&tmp, &macro_index[i], sizeof(macro_idx));
		USB_KeepAlive(true);
	}

	volatile uint16_t zero = 0x0;
	serial_eeprom_write((uint8_t*)macros_end_offset, (uint8_t*)&zero, sizeof(uint16_t));
}

// comparator for a macro key (in ram) and macro_idx (in eeprom)
static int macro_index_cmp(const macro_key* k, const macro_idx* v){
	for(uint8_t j = 0; j < MACRO_MAX_KEYS; ++j){
		int d = k->keys[j] - eeprom_read_byte(&v->keys[j]);
		if(d) return d;
	}
	return 0;
}

static inline macro_idx* find_macro(macro_key* key){
	macro_idx* r = (macro_idx*) bsearch(key,
										macro_index,
										MACRO_INDEX_COUNT, //nelem
										sizeof(macro_idx), // width
										(int(*)(const void*, const void*)) macro_index_cmp);
	return r;
}

// convenience macro for reading from eeprom and handling errors
#define seeprom_read_var(var, ptr)										\
	if(serial_eeprom_read((uint8_t*)ptr, (uint8_t*)&var, sizeof(typeof(var))) != sizeof(typeof(var))){ goto err; }

#define seeprom_write_var(ptr, var)										\
	if(serial_eeprom_write((uint8_t*)ptr, (uint8_t*)&var, sizeof(typeof(var))) != sizeof(typeof(var))){ goto err; }
/**
 * internal function: remove the macro data for the given
 * macro index entry.
 */
static bool delete_macro_data(macro_idx* entry_idx){
	uint16_t entry_offset = eeprom_read_word(&entry_idx->macro_offset);
	macro_data* entry = (macro_data*) (&macros[entry_offset]);

	// read the macro space end offset
	uint16_t end_offset;
	seeprom_read_var(end_offset, macros_end_offset);

	// Read the length of the macro to be deleted
	uint16_t entry_len;
	seeprom_read_var(entry_len, &entry->length);
	entry_len += 2; // length header

	uint16_t rest_offset = entry_offset + entry_len;
	uint16_t rest_len = end_offset - rest_offset;
	if(rest_len){
		// if there is data after the entry, move rest_len bytes of
		// macro data down to entry_offset from rest_offset
		if(serial_eeprom_memmove(&macros[entry_offset], &macros[rest_offset], rest_len) != SUCCESS){ goto err; }
	}
	// and update the saved macro end offset
	end_offset -= entry_len;
	seeprom_write_var(macros_end_offset, end_offset);

	// Now scan the macro index, and move down any entries > entry_offset by entry_len
	for(uint8_t i = 0; i < MACRO_INDEX_COUNT; ++i){
		if(eeprom_read_byte(&macro_index[i].keys[0]) == NO_KEY) break;

		uint16_t ioff = eeprom_read_word(&macro_index[i].macro_offset);
		if(ioff > entry_offset){
			eeprom_update_word(&macro_index[i].macro_offset, ioff - entry_len);
			USB_KeepAlive(true);
		}
	}
	return true;
 err:
	return false;
}

static void remove_macro_index(macro_idx* mi){
	// move macros down into the slot until we hit the end of the array or an empty (keys[0] == NO_KEY) entry
	uint8_t mi_offset = mi - macro_index;
	for(uint8_t i = mi_offset; i < MACRO_INDEX_COUNT; ++i){
		macro_idx tmp;
		if(i == MACRO_INDEX_COUNT - 1){
			// hit the end, fill with empty
			memset(tmp.keys, NO_KEY, MACRO_MAX_KEYS);
			tmp.macro_offset = 0;
		}
		else{
			eeprom_read_block(&tmp, &macro_index[i + 1], sizeof(macro_idx));
		}
		// and write
		eeprom_update_block(&tmp, &macro_index[i], sizeof(macro_idx));

		if(tmp.keys[0] == NO_KEY) break; // done
		USB_KeepAlive(true);
	}
}

/**
 * Looks up a macro based on a set of pressed keys, returns a pointer
 * to the macro data or NO_MACRO if not found.
 */
macro_data* macros_lookup(macro_key* key){
	if(key->keys[0] == NO_KEY){
		return NO_MACRO; // do not allow lookup of empty entry
	}
	macro_idx* r = find_macro(key);
	if(r){
		uint16_t off = eeprom_read_word(&r->macro_offset);
		return (macro_data*) (&macros[off]);
	}
	else return NO_MACRO;
}

/**
 * Starts recording a macro identified by the given key. Adds it to
 * the index, removes any existing data, and returns a pointer to the
 * macro data. Only one macro may be being recorded at once.
 */
bool macros_start_macro(macro_key* key){
	macro_idx* r = find_macro(key);
	if(r){
		// macro index exists: remove the old macro data and re-use this slot
		if(!delete_macro_data(r)) goto err;
	}
	else{
		// macro index does not exist: move the index up from the end until we reach
		// a key lower than us, then insert
		if(eeprom_read_byte(&macro_index[MACRO_INDEX_COUNT - 1].keys[0]) != NO_KEY){
			// then we're full, error
			goto err;
		}
		for(int i = MACRO_INDEX_COUNT - 1; i >= 0; --i){
			// consider each slot i from the end. If it is the correct
			// position (first or key is >= the preceding cell) then
			// store, otherwise move the value in preceding cell up
			// and repeat.
			if(i == 0 || macro_index_cmp(key, &macro_index[i-1]) >= 0){
				r = &macro_index[i];
				break;
			}
			else if(eeprom_read_byte(&macro_index[i-1].keys[0]) == NO_KEY){
				continue; // Don't bother to copy empty cells.
			}
			else{
				// copy up (i-1)
				macro_idx tmp;
				eeprom_read_block  (&tmp, &macro_index[i-1], sizeof(macro_idx));
				eeprom_update_block(&tmp, &macro_index[i],   sizeof(macro_idx));
				USB_KeepAlive(true);
			}
		}
	}
	// we now have a free index cell at r: write in the key part
	eeprom_update_block(key, r, sizeof(macro_key)); // macro_key is prefix to macro_idx
	USB_KeepAlive(true);

	// and now the data
	uint16_t new_macro_offset;
	seeprom_read_var(new_macro_offset, macros_end_offset);
	eeprom_update_word((uint16_t*)&r->macro_offset, (uint16_t)new_macro_offset);

	// and set up the new macro for recording content
	current_macro = (macro_data*) &macros[new_macro_offset];
	current_macro_cursor = &current_macro->events[0];
	current_macro_index = r;
	return true;

 err:
	buzzer_start_f(200, BUZZER_FAILURE_TONE);
	current_macro = 0;
	current_macro_cursor = 0;
	current_macro_index = 0;
	return false;
}

/**
 * Commits the current macro which was started with
 * macros_start_macro().  If len is 0, instead rolls back the macro
 * creation and removes the entry from the index.  This is also used
 * to delete a macro.
 */
void macros_commit_macro(){
	if(current_macro == 0){
		// cannot commit no macro
		goto err;
	}
	uint16_t macro_len = current_macro_cursor - &current_macro->events[0];
	if(macro_len == 0){
		// find the macro in the index and remove it.
		remove_macro_index(current_macro_index);
	}
	else{
		seeprom_write_var(&current_macro->length, macro_len);
		uint16_t end_offset;
		seeprom_read_var(end_offset, macros_end_offset);
		end_offset += macro_len + 2; // length header + data
		seeprom_write_var(macros_end_offset, end_offset);
		buzzer_start_f(200, BUZZER_SUCCESS_TONE);
	}

	current_macro = 0;
	current_macro_cursor = 0;
	current_macro_index = 0;
	return;

 err:
	buzzer_start_f(200, BUZZER_FAILURE_TONE);
}

void macros_abort_macro(){
	remove_macro_index(current_macro_index);
	current_macro = 0;
	current_macro_cursor = 0;
	current_macro_index = 0;
}

bool macros_append(hid_keycode event){
	seeprom_write_var(current_macro_cursor++, event);
	return true;
 err:
	return false;
}

bool macros_fill_next_report(macro_playback* state, KeyboardReport_Data_t* report){
	if(state->remaining){
		--state->remaining;
		hid_keycode event;
		seeprom_read_var(event, state->cursor++);
		ExtraKeyboardReport_toggle(&state->report, event);
		ExtraKeyboardReport_append(&state->report, report);
		return true;
	}
	else{
		return false;
	}
 err:
	buzzer_start_f(200, BUZZER_FAILURE_TONE);
	return false;
}
