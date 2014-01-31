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

#include "macro_index.h"
#include "macro.h"

#include "usb.h"
#include "printing.h"
#include "serial_eeprom.h"
#include "buzzer.h"

#include <stdint.h>
#include <stdlib.h>
#include <avr/eeprom.h>
#include <util/delay.h>

// The macro data itself is in external eeprom
static uint8_t macros_storage[MACROS_SIZE] EEEXT;
static uint16_t *const macros_end_offset = (uint16_t*)macros_storage;
static uint8_t  *const macros = macros_storage + sizeof(uint16_t);


/////////////// Recording and Playback Data ////////////////////

static struct _macro_recording_state {
	macro_data* macro;
	hid_keycode* cursor;
	macro_idx_entry* index_entry;
} recording_state;

static struct _macro_playback_state {
	uint16_t remaining;
	hid_keycode* cursor; // pointer to serial eeprom memory
	ExtraKeyboardReport report;
} playback_state;

////////////////////// Macro Management ////////////////////////

uint8_t* macros_get_storage(){
	return &macros_storage[0];
}

void macros_reset_defaults(){
	volatile uint16_t zero = 0x0;
	serial_eeprom_write((uint8_t*)macros_end_offset, (uint8_t*)&zero, sizeof(uint16_t));
}

static macro_data* macros_get_macro_pointer(uint16_t offset){
	return (macro_data*) &macros[offset];
}

// convenience macro for reading from eeprom and handling errors
#define seeprom_read_var(var, ptr)										\
	if(serial_eeprom_read((uint8_t*)ptr, (uint8_t*)&var, sizeof(typeof(var))) != sizeof(typeof(var))){ goto err; }

#define seeprom_write_var(ptr, var)										\
	if(serial_eeprom_write((uint8_t*)ptr, (uint8_t*)&var, sizeof(typeof(var))) != sizeof(typeof(var))){ goto err; }


typedef struct {
	uint16_t offset;
	uint16_t len;
} macro_range;

static void macro_shift_down_iterator(macro_idx_entry* entry, macro_range* range){
	macro_idx_entry_data d = macro_idx_get_data(entry);
	if(d.type != MACRO) return;
	// offset of start of macro is in d.data: if it's greater than the
	// removed range, subtract the removed length
	if(d.data > range->offset){
		d.data -= range->len;
		macro_idx_set_data(entry, d);
	}
}

/**
 * internal function: remove any macro data for the given macro index
 * entry. Returns true if no error, false if error.
 */
static bool delete_macro_data(macro_idx_entry* idx_entry){
	macro_idx_entry_data idx_data = macro_idx_get_data(idx_entry);
	if(idx_data.type != MACRO) return true; // no data to delete, trivial success

	uint16_t entry_offset = idx_data.data;
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

		// Now scan the macro index, and move down any entries > entry_offset by entry_len
		macro_range deleted_range;
		deleted_range.offset = entry_offset;
		deleted_range.len = entry_len;
		macro_idx_iterate((macro_idx_iterator)macro_shift_down_iterator, &deleted_range);
	}
	// and update the saved macro end offset
	end_offset -= entry_len;
	seeprom_write_var(macros_end_offset, end_offset);

	return true;
 err:
	return false;
}


/////////// Macro Recording /////////////

/**
 * Starts recording a macro identified by the given key. Adds it to
 * the index, removes any existing data, and returns a pointer to the
 * macro data. Only one macro may be being recorded at once.
 */
bool macros_start_macro(macro_idx_key* key){
	// Find or create a free entry:
	macro_idx_entry* entry = macro_idx_lookup(key);
	if(entry){
		// There's already an entry for this key in the index: delete
		// the old macro data if necessary and re-use this slot
		if(!delete_macro_data(entry)) goto err;
	}
	else{
		entry = macro_idx_create(key);
		if(!entry) goto err;
	}

	// Now store the data in the entry:
	macro_idx_entry_data new_entry_data;
	new_entry_data.type = MACRO;
	seeprom_read_var(new_entry_data.data, macros_end_offset);
	macro_idx_set_data(entry, new_entry_data);

	// and set up the new macro for recording content
	recording_state.macro = macros_get_macro_pointer(new_entry_data.data);
	recording_state.cursor = &recording_state.macro->events[0];
	recording_state.index_entry = entry;
	return true;

 err:
	buzzer_start_f(200, BUZZER_FAILURE_TONE);
	memset(&recording_state, 0x0, sizeof(recording_state));
	return false;
}

/**
 * Commits the current macro which was started with
 * macros_start_macro().  If len is 0, instead rolls back the macro
 * creation and removes the entry from the index.  This is also used
 * to delete a macro.
 */
void macros_commit_macro(){
	if(!recording_state.macro){
		// cannot commit no macro
		goto err;
	}
	uint16_t macro_len = recording_state.cursor - &recording_state.macro->events[0];
	if(macro_len == 0){
		// find the macro in the index and remove it.
		macro_idx_remove(recording_state.index_entry);
	}
	else{
		seeprom_write_var(&recording_state.macro->length, macro_len);
		uint16_t end_offset;
		seeprom_read_var(end_offset, macros_end_offset);
		end_offset += macro_len + 2; // length header + data
		seeprom_write_var(macros_end_offset, end_offset);
		buzzer_start_f(200, BUZZER_SUCCESS_TONE);
	}

	memset(&recording_state, 0x0, sizeof(recording_state));
	return;

 err:
	buzzer_start_f(200, BUZZER_FAILURE_TONE);
}

void macros_abort_macro(){
	macro_idx_remove(recording_state.index_entry);
	memset(&recording_state, 0x0, sizeof(recording_state));
}

bool macros_append(hid_keycode event){
	seeprom_write_var(recording_state.cursor++, event);
	return true;
 err:
	return false;
}

////// Macro Playback /////


bool macros_start_playback(uint16_t macro_offset){
	macro_data* macro = macros_get_macro_pointer(macro_offset);
	ExtraKeyboardReport_clear(&playback_state.report);
	playback_state.cursor = &macro->events[0];
	seeprom_read_var(playback_state.remaining, &macro->length);
	return true;

 err:
	return false;
}

bool macros_fill_next_report(KeyboardReport_Data_t* report){
	if(playback_state.remaining){
		--playback_state.remaining;
		hid_keycode event;
		seeprom_read_var(event, playback_state.cursor++);
		ExtraKeyboardReport_toggle(&playback_state.report, event);
		ExtraKeyboardReport_append(&playback_state.report, report);
	}
	return playback_state.remaining ? true : false;
 err:
	buzzer_start_f(200, BUZZER_FAILURE_TONE);
	return false;
}
