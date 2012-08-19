/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  This file is offered under either of the GNU GPL v2 or MIT licences
  below in order that it may be used with either of the V-USB or LUFA
  USB libraries.

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

#include "config.h"

#include "Keyboard.h"
#include "hardware.h"
#include "printing.h"
#include "keystate.h"
#include "leds.h"

#include "avr/eeprom.h"

// Eeprom sentinel value - if this is not set at startup, re-initialize the eeprom.
#define EEPROM_SENTINEL 42
uint8_t eeprom_sentinel_byte EEMEM;

// Persistent configuration (e.g. sound enabled)
configuration_flags eeprom_flags EEMEM;

// Key configuration is stored in eeprom. If the sentinel is not valid, initialize from the defaults.
hid_keycode logical_to_hid_map[NUM_LOGICAL_KEYS] EEMEM;

// We support saving up to 10 key mappings as their difference from the default.
#define NUM_KEY_MAPPING_INDICES 10
struct { uint8_t start; uint8_t end; } saved_key_mapping_indices[NUM_KEY_MAPPING_INDICES] EEMEM;

// Key mappings are saved as a list of (logical_keycode, hid_keycode) pairs.
#define SAVED_KEY_MAPPINGS_BUFFER_SIZE 128
struct { logical_keycode l_key; hid_keycode h_key; } saved_key_mappings[SAVED_KEY_MAPPINGS_BUFFER_SIZE] EEMEM;

hid_keycode config_get_definition(logical_keycode l_key){
	return eeprom_read_byte(&logical_to_hid_map[l_key]);
}

void config_save_definition(logical_keycode l_key, hid_keycode h_key){
	eeprom_update_byte(&logical_to_hid_map[l_key], h_key);
}

// reset the current layout to the default layout
void config_reset_defaults(void){
	for(int i = 0; i < NUM_LOGICAL_KEYS; ++i){
		hid_keycode default_key = pgm_read_byte_near(&logical_to_hid_map_default[i]);
		eeprom_update_byte(&logical_to_hid_map[i], default_key);
	}
	eeprom_update_byte((uint8_t*)&eeprom_flags, 0x0);
	// flash LEDs to show that we had to reset
	leds_blink();
}

// reset the keyboard, including saved layouts
void config_reset_fully(void){
	eeprom_update_byte(&eeprom_sentinel_byte, EEPROM_SENTINEL);
	for(int i = 0; i < NUM_KEY_MAPPING_INDICES; ++i){
		eeprom_update_byte(&saved_key_mapping_indices[i].start, NO_KEY);
	}
	config_reset_defaults();
}

configuration_flags config_get_flags(void){
	union {
		uint8_t b;
		configuration_flags s;
	} r;
	r.b = eeprom_read_byte((uint8_t*)&eeprom_flags);
	return r.s;
}

void config_save_flags(configuration_flags state){
	union {
		uint8_t b;
		configuration_flags s;
	} r;
	r.s = state;
	eeprom_update_byte((uint8_t*)&eeprom_flags, r.b);
}


static const char MSG_NO_LAYOUT[] PROGMEM = "No such layout";

uint8_t config_delete_layout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		printing_set_buffer(MSG_NO_LAYOUT);
		return false;
	}
	uint8_t start = eeprom_read_byte(&saved_key_mapping_indices[num].start);
	if(start == NO_KEY){
		printing_set_buffer(MSG_NO_LAYOUT);
		return false;
	}
	uint8_t end = eeprom_read_byte(&saved_key_mapping_indices[num].end); // start and end are inclusive

	uint8_t length = start - end + 1;

	// clear this entry
	eeprom_update_byte(&saved_key_mapping_indices[num].start, NO_KEY);

	// now scan the other entries, subtracting length from each entry indexed after end
	// update the end position so we can move down only necessary data.
	uint8_t max_end = end;
	for(int i = 0; i < NUM_KEY_MAPPING_INDICES; ++i){
		uint8_t i_start = eeprom_read_byte(&saved_key_mapping_indices[i].start);
		if(i_start != NO_KEY && i_start > end){
			uint8_t i_end = eeprom_read_byte(&saved_key_mapping_indices[i].end);
			if(i_end > max_end) max_end = i_end;

			eeprom_update_byte(&saved_key_mapping_indices[i].start, i_start - length);
			eeprom_update_byte(&saved_key_mapping_indices[i].end,   i_end - length);
		}
	}

	// and move down the data.
	for(int i = end+1; i <= max_end; ++i){
		uint8_t lk = eeprom_read_byte(&saved_key_mappings[i].l_key);
		uint8_t hk = eeprom_read_byte(&saved_key_mappings[i].h_key);
		eeprom_update_byte(&saved_key_mappings[i - length].l_key, lk);
		eeprom_update_byte(&saved_key_mappings[i - length].h_key, hk);
	}

	return true;
}

uint8_t config_save_layout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		printing_set_buffer(MSG_NO_LAYOUT);
		return false;
	}

	// remove old layout
	config_delete_layout(num);

	// find last offset
	uint8_t old_end = 0;
	for(int i = 0; i < NUM_KEY_MAPPING_INDICES; ++i){
		uint8_t i_start = eeprom_read_byte(&saved_key_mapping_indices[i].start);
		if(i_start == NO_KEY) continue;
		uint8_t i_end = eeprom_read_byte(&saved_key_mapping_indices[i].end);
		if(i_end > old_end) old_end = i_end;
	}

	uint8_t start = old_end + 1;
	uint8_t cursor = start;

	for(logical_keycode l = 0; l < NUM_LOGICAL_KEYS; ++l){
		hid_keycode h = eeprom_read_byte(&logical_to_hid_map[l]);
		hid_keycode d = pgm_read_byte_near(&logical_to_hid_map_default[l]);
		if(h != d){
			if(cursor >= SAVED_KEY_MAPPINGS_BUFFER_SIZE - 1){
				static const char msg[] PROGMEM = "Out of space, can't save layout.";
				printing_set_buffer(msg);
				return false; // no space!
			}
			eeprom_update_byte(&saved_key_mappings[cursor].l_key, l);
			eeprom_update_byte(&saved_key_mappings[cursor].h_key, h);
			++cursor;
		}
	}
	if(start != cursor){
		eeprom_update_byte(&saved_key_mapping_indices[num].start, start);
		eeprom_update_byte(&saved_key_mapping_indices[num].end,   cursor - 1);
		return true;
	}
	else{
		static const char msg[] PROGMEM = "No changes, not saved.";
		printing_set_buffer(msg);
		return false;
	}
}

uint8_t config_load_layout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		printing_set_buffer(MSG_NO_LAYOUT);
		return false;
	}

	uint8_t start = eeprom_read_byte(&saved_key_mapping_indices[num].start);
	if(start == NO_KEY){
		printing_set_buffer(MSG_NO_LAYOUT);
		return false;
	}
	uint8_t end = eeprom_read_byte(&saved_key_mapping_indices[num].end);

	uint8_t offset = start;

	logical_keycode next_key = eeprom_read_byte(&saved_key_mappings[offset].l_key);
	logical_keycode next_val = eeprom_read_byte(&saved_key_mappings[offset].h_key);
	++offset;

	for(logical_keycode lkey = 0; lkey < NUM_LOGICAL_KEYS; ++lkey){
		if(lkey != next_key){
			// use default
			hid_keycode def_val = pgm_read_byte_near(&logical_to_hid_map_default[lkey]);
			eeprom_update_byte(&logical_to_hid_map[lkey], def_val);
		}
		else{
			// use saved
			eeprom_update_byte(&logical_to_hid_map[lkey], next_val);
			if(offset <= end){
				next_key = eeprom_read_byte(&saved_key_mappings[offset].l_key);
				next_val = eeprom_read_byte(&saved_key_mappings[offset].h_key);
				++offset;
			}
		}
	}

	return true;
}

void config_init(void){
	uint8_t sentinel = eeprom_read_byte(&eeprom_sentinel_byte);
	if(sentinel != EEPROM_SENTINEL){
		config_reset_fully();
	}
}
