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

#include <stdint.h>

#include "hardware.h"

#include "macro_index.h"
#include "macro.h"

#include "usb.h"
#include "printing.h"
#include "serial_eeprom.h"
#include "buzzer.h"
#include "config.h"
#include "sort.h"

#include <stdint.h>
#include <stdlib.h>
#include <avr/eeprom.h>
#include <util/delay.h>

// Storage format
typedef struct _macro_idx_entry {
	// unused keycodes are NO_KEY (so an empty entry starts with NO_KEY)
	hid_keycode keys[MACRO_MAX_KEYS];
	uint16_t val; // high bit indicates whether the entry is a macro
				  // or a program. If macro, remaining bits are the
				  // offset to the macro_data in macros array
				  // (obtained via macros_get_macro_offset). If
				  // program, then they are the integer program id.
} macro_idx_entry;

// The macro lookup index is in internal eeprom
#define MACRO_INDEX_COUNT (MACRO_INDEX_SIZE / sizeof(macro_idx_entry))
static macro_idx_entry macro_index[MACRO_INDEX_COUNT] EEMEM;

/**
 * Get a pointer to the underlying data in EEMEM. (To be read/written
 * as a whole by the client application)
 */
uint8_t* macro_idx_get_storage(){
	return (uint8_t*) &macro_index[0];
}

/**
 * Erases the macro index - to be called from config_reset_fully
 */
void macro_idx_reset_defaults(){
	macro_idx_entry tmp;
	memset(tmp.keys, NO_KEY, MACRO_MAX_KEYS);
	tmp.val = 0x0;

	for(uint8_t i = 0; i < MACRO_INDEX_COUNT; ++i){
		eeprom_update_block(&tmp, &macro_index[i], sizeof(macro_idx_entry));
		USB_KeepAlive(true);
	}
}

bool macro_idx_format_key(macro_idx_key* key, uint8_t key_count){
	// Keypad shift can't be included in a macro trigger if we want macros in
	// the keypad layer to work with both shift and toggle.
	for(uint8_t i = 0; i < key_count; ++i){
		if(config_get_definition(key->keys[i]) == SPECIAL_HID_KEY_KEYPAD_SHIFT){
			key->keys[i] = NO_KEY;
		}
	}
	insertionsort_uint8(key->keys, key_count);

	for(uint8_t i = key_count; i < MACRO_MAX_KEYS; ++i){
		key->keys[i] = NO_KEY;
	}

	return key->keys[0] != NO_KEY;
}

// Internal management functions:

// comparator for a macro key (in ram) and macro_idx_entry (in eeprom)
static int macro_idx_cmp(const macro_idx_key* k, const macro_idx_entry* v){
	for(uint8_t j = 0; j < MACRO_MAX_KEYS; ++j){
		int d = k->keys[j] - eeprom_read_byte(&v->keys[j]);
		if(d) return d;
	}
	return 0;
}

/** returns pointer to EEMEM */
macro_idx_entry* macro_idx_lookup(macro_idx_key* key){
	macro_idx_entry* r =
		(macro_idx_entry*) bsearch(key,
								   macro_index,
								   MACRO_INDEX_COUNT, //nelem
								   sizeof(macro_idx_entry), // width
								   (int(*)(const void*, const void*)) macro_idx_cmp);
	return r;
}

macro_idx_entry_data macro_idx_get_data(macro_idx_entry* mh){
	macro_idx_entry_data r;
	uint16_t val = eeprom_read_word(&mh->val);
	r.type = (val & 0x8000) ? PROGRAM : MACRO;
	r.data = val & 0x7fff;
	return r;
}

void macro_idx_set_data(macro_idx_entry* mh, macro_idx_entry_data d){
	uint16_t store = d.data;
	if(d.type == PROGRAM){
		store |= 0x8000;
	}
	eeprom_update_word(&mh->val, store);
	USB_KeepAlive(true);
}

/**
 * Removes an entry from the index
 */
void macro_idx_remove(macro_idx_entry* mi){
	// move macros down into the slot until we hit the end of the array or an empty (keys[0] == NO_KEY) entry
	uint8_t mi_offset = mi - macro_index;
	for(uint8_t i = mi_offset; i < MACRO_INDEX_COUNT; ++i){
		macro_idx_entry tmp;
		if(i == MACRO_INDEX_COUNT - 1){
			// hit the end, fill with empty rather than moving i+1
			memset(tmp.keys, NO_KEY, MACRO_MAX_KEYS);
			tmp.val = 0x0;
		}
		else{
			eeprom_read_block(&tmp, &macro_index[i + 1], sizeof(macro_idx_entry));
		}
		// and write into i
		eeprom_update_block(&tmp, &macro_index[i], sizeof(macro_idx_entry));

		// We're done if we've just filled with an empty entry
		if(tmp.keys[0] == NO_KEY) break; // done
		USB_KeepAlive(true);
	}
}

/**
 * Adds an entry for the given key to the index, and returns a pointer
 * to the new (empty) entry, or NULL if full or error.
 */
macro_idx_entry* macro_idx_create(macro_idx_key* key){
	macro_idx_entry* r;
	// macro index does not exist: move the index up from the end until we reach
	// a key lower than us, then insert
	if(eeprom_read_byte(&macro_index[MACRO_INDEX_COUNT - 1].keys[0]) != NO_KEY){
		// then we're full, error
		return NULL;
	}
	for(int i = MACRO_INDEX_COUNT - 1; i >= 0; --i){
		// consider each slot i from the end. If it is the correct
		// position (first or key is >= the preceding cell) then
		// store, otherwise move the value in preceding cell up
		// and repeat.
		if(i == 0 || macro_idx_cmp(key, &macro_index[i-1]) >= 0){
			r = &macro_index[i];
			break;
		}
		else if(eeprom_read_byte(&macro_index[i-1].keys[0]) == NO_KEY){
			continue; // Don't bother to copy empty cells.
		}
		else{
			// copy up (i-1) to (i), leaving the hole at (i-1)
			macro_idx_entry tmp;
			eeprom_read_block  (&tmp, &macro_index[i-1], sizeof(macro_idx_entry));
			eeprom_update_block(&tmp, &macro_index[i],   sizeof(macro_idx_entry));
			USB_KeepAlive(true);
		}
	}

	// we now have a correclty positioned index cell at r: write in the key
	eeprom_update_block(key, r, sizeof(macro_idx_key)); // macro_idx_key is prefix to macro_idx
	USB_KeepAlive(true);
	return r;
}

void macro_idx_iterate(macro_idx_iterator itr, void* c){
	for(uint8_t i = 0; i < MACRO_INDEX_COUNT; ++i){
		if(eeprom_read_byte(&macro_index[i].keys[0]) == NO_KEY) break;
		itr(&macro_index[i], c);
	}
}
