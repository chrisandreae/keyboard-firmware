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
#include "serial_eeprom.h"
#include "interpreter.h"
#include "buzzer.h"

#include "avr/eeprom.h"

// Eeprom sentinel value - if this is not set at startup, re-initialize the eeprom.
#define EEPROM_SENTINEL 42
uint8_t eeprom_sentinel_byte EEMEM;

// Persistent configuration (e.g. sound enabled)
configuration_flags eeprom_flags EEMEM;

// Key configuration is stored in eeprom. If the sentinel is not valid, initialize from the defaults.
hid_keycode logical_to_hid_map[NUM_LOGICAL_KEYS] EEMEM;

hid_keycode* config_get_mapping(){
	return &logical_to_hid_map[0];
}

// We support saving up to 10 keyboard remappings as their differences from the default.
// These (variable sized) mappings are stored in the fixed-size buffer saved_key_mappings,
// indexed by saved_key_mapping_indices. The buffer is kept packed (subsequent mappings
// moved down on removal)
#define NUM_KEY_MAPPING_INDICES 10
struct { uint8_t start; uint8_t end; } saved_key_mapping_indices[NUM_KEY_MAPPING_INDICES] EEMEM;

// Key mappings are saved as a list of (logical_keycode, hid_keycode)
// pairs, approximately filling the remaining internal eeprom.
#define SAVED_KEY_MAPPINGS_BUFFER_SIZE 384
struct { logical_keycode l_key; hid_keycode h_key; } saved_key_mappings[SAVED_KEY_MAPPINGS_BUFFER_SIZE] EEMEM;

#if USE_EEPROM

// Programs are stored in external eeprom. We dedicate 1k of our 2k
// eeprom for program storage, leaving the rest for keyboard macros.

static uint8_t programs[PROGRAMS_SIZE] EEEXT;

typedef struct _program_idx { uint16_t offset; uint16_t len; } program_idx;
static program_idx *const programs_index = (uint16_t*) programs;

static uint8_t *const programs_data = programs + (NUM_PROGRAMS * sizeof(program_idx));

static uint8_t macros[MACROS_SIZE] EEEXT; // as yet unused

uint8_t* config_get_programs(){
	return &programs[0];
}

uint8_t* config_get_macros(){
	return &macros[0];
}
#endif


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

	// Buzz to signify reset (TODO: also flash LEDs)
#if USE_BUZZER
	buzzer_start(300);
#endif
}

// reset the keyboard, including saved layouts
void config_reset_fully(void){
	eeprom_update_byte(&eeprom_sentinel_byte, EEPROM_SENTINEL);

	{
		// reset key mapping index
		const uint8_t sz = sizeof(saved_key_mapping_indices) * sizeof(*saved_key_mapping_indices);
		uint8_t buf[sz];
		memset(buf, NO_KEY, sz);
		eeprom_update_block(buf, saved_key_mapping_indices, sz);
	}

#if USE_EEPROM
	{
	// reset program index
		uint8_t sz = NUM_PROGRAMS * sizeof(program_idx);

		uint8_t buf[EEEXT_PAGE_SIZE];
		memset(buf, NO_KEY, EEEXT_PAGE_SIZE);

		uint8_t* p = (uint8_t*) programs_index;
		while(sz > 0){
			uint8_t step = (sz > EEEXT_PAGE_SIZE) ? EEEXT_PAGE_SIZE : sz;
			serial_eeprom_write_page(p, buf, step);
			p += step;
			sz -= step;
		}
	}
#endif // USE_EEPROM

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


static const char MSG_NO_LAYOUT[] PROGMEM = "No layout";

uint8_t config_delete_layout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		printing_set_buffer(MSG_NO_LAYOUT, BUF_PGM);
		return false;
	}
	uint8_t start = eeprom_read_byte(&saved_key_mapping_indices[num].start);
	if(start == NO_KEY){
		printing_set_buffer(MSG_NO_LAYOUT, BUF_PGM);
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
		printing_set_buffer(MSG_NO_LAYOUT, BUF_PGM);
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
				printing_set_buffer(PGM_MSG("Fail: no space"), BUF_PGM);
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
		// same as default layout: nothing to save.
		return false;
	}
}

uint8_t config_load_layout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		printing_set_buffer(MSG_NO_LAYOUT, BUF_PGM);
		return false;
	}

	uint8_t start = eeprom_read_byte(&saved_key_mapping_indices[num].start);
	if(start == NO_KEY){
		printing_set_buffer(MSG_NO_LAYOUT, BUF_PGM);
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

#if USE_EEPROM
const program* config_get_program(uint8_t idx){
	//index range is not checked as this can't be called from user input
	uint16_t program_offset;
	if(-1 == serial_eeprom_read((uint8_t*)&programs_index[idx].offset,
								(uint8_t*)&program_offset,
								sizeof(uint16_t))){
		return 0;
	}
	if(program_offset == 0xffff){
		return 0;
	}
	return (const program*) &programs_data[program_offset];
}
#endif // USE_EEPROM

void config_init(void){
	uint8_t sentinel = eeprom_read_byte(&eeprom_sentinel_byte);
	if(sentinel != EEPROM_SENTINEL){
		config_reset_fully();
	}
}
