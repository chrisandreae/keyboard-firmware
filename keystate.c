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

#include "keystate.h"

#include "Keyboard.h"
#include "hardware.h"
#include "config.h"
#include "buzzer.h"
#include "interpreter.h"
#include "storage.h"

#include <stdarg.h>

// State of active keys. Keep track of all pressed or debouncing keys.
static key_state key_states[NUM_PHYSICAL_KEYS];

static keystate_change_hook keystate_change_hook_fn = 0;

uint8_t key_press_count = 0;

struct {
	unsigned char toggle:1;
	unsigned char shift_count:7;
} keypad_state;

void keystate_init(void){
	for(physical_keycode k = 0; k < NUM_PHYSICAL_KEYS; ++k){
		key_states[k].state    = 0;
		key_states[k].valid    = 1;
		key_states[k].debounce = 0;
	}
}

bool keystate_is_keypad_mode(void){
	return (keypad_state.toggle != 0) || (keypad_state.shift_count != 0);
}

static inline logical_keycode keystate_apply_keypad_layer(physical_keycode key, bool layer){
	if(layer){
		key += KEYPAD_LAYER_SIZE;
	}
	return key;
}

static inline void keystate_set_key(physical_keycode key){
	++key_press_count;
	key_states[key].state = 1;
	key_states[key].valid = 1;

	if(keystate_change_hook_fn){
		logical_keycode l_key = keystate_apply_keypad_layer(key, keystate_is_keypad_mode());
		keystate_change_hook_fn(l_key, true);
	}

	#if USE_BUZZER
	if(config_get_flags().key_sound_enabled)
		buzzer_start(3);
	#endif
}

static inline uint8_t keystate_clear_key(physical_keycode key){
	key_state old_state = key_states[key];

	key_states[key].state = 0;
	key_states[key].valid = 1;

	if(old_state.state && old_state.valid){ // if it had been pressed
		key_press_count--;
		if(keystate_change_hook_fn){
			logical_keycode l_key = keystate_apply_keypad_layer(key, keystate_is_keypad_mode());
			keystate_change_hook_fn(l_key, false);
		}
		return 1;
	}
	else {
		return 0;
	}
}

// Called when a keypad key (either toggle or shift) changes state. If the key
// change causes the keypad mode to be toggled, all currently tracked keys that
// are no longer valid in the new mode are reset. Returns true if the keypad
// state changed.
static uint8_t keystate_update_keypad(hid_keycode keypad_key, uint8_t state){
	bool prev_keypad_mode = keystate_is_keypad_mode();

	switch(keypad_key){
	case SPECIAL_HID_KEY_KEYPAD_TOGGLE:
		// Toggle keypad mode on keydown
		if(state) { keypad_state.toggle = !keypad_state.toggle; }
		break;
	case SPECIAL_HID_KEY_KEYPAD_SHIFT:
		if(state) { ++keypad_state.shift_count; }
		else      { --keypad_state.shift_count; }
		break;
	default:
		return false;
	}

	uint8_t keypad_mode = keystate_is_keypad_mode();

	if(prev_keypad_mode == keypad_mode){ return false; }

	// Otherwise, invalidate all currently tracked keys that are now no longer available
	for(physical_keycode p_key = 0; p_key < NUM_PHYSICAL_KEYS; ++p_key){
		// ignore untracked and invalidated
		if(key_states[p_key].state == 0 || key_states[p_key].valid == 0){
			continue;
		}
		logical_keycode prev_key = keystate_apply_keypad_layer(p_key, prev_keypad_mode);
		hid_keycode prev_h_key   = config_get_definition(prev_key);

		// ignore special keys that don't participate in layers
		if(SPECIAL_HID_KEY_NOREMAP(prev_h_key)){
			continue;
		}

		logical_keycode new_key  = keystate_apply_keypad_layer(p_key, keypad_mode);
		hid_keycode new_h_key    = config_get_definition(new_key);

		// if the tracked key is valid in the new mode, continue
		if(prev_h_key == new_h_key){
			continue;
		}

		// otherwise invalidate
		key_states[p_key].valid = 0;
		key_press_count--;
		if(keystate_change_hook_fn){
			logical_keycode l_key = keystate_apply_keypad_layer(p_key, prev_keypad_mode);
			keystate_change_hook_fn(l_key, false);
		}
	}
	return true;
}



void keystate_update(void){
	// for each entry i in the matrix
	for(uint8_t matrix_row = 0; matrix_row < MATRIX_ROWS; ++matrix_row){
		matrix_select_row(matrix_row);

		for(uint8_t matrix_col = 0; matrix_col < MATRIX_COLS; ++matrix_col){

			// look up the physical keycode for the matrix code
			// Note that only one matrix position should map to any given code:
			//  otherwise we won't register a keypress unless both are pressed:
			//  one position will be debouncing up and the other down.
			physical_keycode p_key = storage_read_byte(CONSTANT_STORAGE, &matrix_to_logical_map[matrix_row][matrix_col]);
			if(p_key == NO_KEY) continue; // empty space in the sparse matrix

			hid_keycode h_key = config_get_definition(p_key);
			bool noremap_key = SPECIAL_HID_KEY_NOREMAP(h_key);

			uint8_t reading = matrix_read_column(matrix_col);
			key_state* key = &key_states[p_key];

			key->debounce = DEBOUNCE_MASK & ((key->debounce << 1) | reading);

			if(key->state == 1 && key->debounce == 0x00){
				// key is not pressed (either debounced-down or never made it up), remove it
				uint8_t old_state = keystate_clear_key(p_key);
				if(old_state && noremap_key){
					uint8_t changed = keystate_update_keypad(h_key, false);
					if(changed){ matrix_col = 0; matrix_row = 0; }
				}
			}
			else if(key->state == 0 && key->debounce == DEBOUNCE_MASK){
				keystate_set_key(p_key);
				if(noremap_key){
					uint8_t changed = keystate_update_keypad(h_key, true);
					if(changed){ matrix_col = 0; matrix_row = 0; }
				}
			}
		}
	}
}

	// needs rewrite for full state.
	
static inline keycode keystate_process_keycode(physical_keycode raw_key, keycode_type ktype){
	keycode key = raw_key;
	if(ktype != PHYSICAL){
		key = keystate_apply_keypad_layer(key, keystate_is_keypad_mode());
	}
	if(ktype == HID){
		key = config_get_definition(key);
	}
	return key;
}

bool keystate_check_key(keycode target_key, keycode_type ktype){
	for(physical_keycode p_key = 0; p_key < NUM_PHYSICAL_KEYS; ++p_key){
		if(key_states[p_key].state == 1 && key_states[p_key].valid == 1){
			keycode key = keystate_process_keycode(p_key, ktype);
			if(key == target_key){
				return true;
			}
		}
	}
	return false;
}
/** returns true if all argument keys are down */
bool keystate_check_keys(uint8_t count, keycode_type ktype, ...){
	if(count > key_press_count) return false; // trivially know it's impossible

	va_list argp;
	bool success = true;
	va_start(argp, ktype);
	while(count--){
		keycode target_key = va_arg(argp, int);
		bool found_key = keystate_check_key(target_key, ktype);

		if(!found_key){
			success = false;
			break;
		}
	}

	va_end(argp);
	return success;
}

bool keystate_check_any_key(uint8_t count, keycode_type ktype, ...){
	if(key_press_count == 0) return false;

	va_list argp;
	bool success = false;
	va_start(argp, ktype);
	while(count--){
		keycode target_key = va_arg(argp, int);
		bool found_key = keystate_check_key(target_key, ktype);

		if(found_key){
			success = true;
			break;
		}
	}

	va_end(argp);
	return success;
}

/**
 * writes up to key_press_count currently pressed key indexes to the
 * output buffer keys.
 */
void keystate_get_keys(keycode* keys, keycode_type ktype){
	int ki = 0;
	for(physical_keycode p_key = 0; p_key < NUM_PHYSICAL_KEYS; ++p_key){
		if(key_states[p_key].state == 1 && key_states[p_key].valid == 1){
			keycode key = keystate_process_keycode(p_key, ktype);
			keys[ki++] = key;
		}
	}
}

void keystate_Fill_KeyboardReport(KeyboardReport_Data_t* KeyboardReport){
	uint8_t used_key_codes = 0;
	uint8_t rollover = false;
	// check key state
	for(physical_keycode p_key = 0; p_key < NUM_PHYSICAL_KEYS; ++p_key){
		if(key_states[p_key].state == 1 && key_states[p_key].valid == 1){
			if(used_key_codes == KEYBOARDREPORT_KEY_COUNT){
				rollover = true;
				break;
			}

			hid_keycode h_key = keystate_process_keycode(p_key, HID);

			// Simple way to ensure program key combinations never cause typing
			if(h_key == SPECIAL_HID_KEY_PROGRAM) rollover = true;

			// check for special and modifier keys
			if(h_key >= SPECIAL_HID_KEYS_START){
				// There's no output for a special key
				continue;
			}
			else if(h_key >= HID_KEYBOARD_SC_LEFT_CONTROL){
				uint8_t shift = h_key - HID_KEYBOARD_SC_LEFT_CONTROL;
				KeyboardReport->Modifier |= (1 << shift);
			}
			else{
				KeyboardReport->KeyCode[used_key_codes++] = h_key;
			}
		}
	}
	if(rollover){
		for(int i = 0; i < KEYBOARDREPORT_KEY_COUNT; ++i)
			KeyboardReport->KeyCode[i] = HID_KEYBOARD_SC_ERROR_ROLLOVER;
	 }
}

static inline uint8_t ilog2_16(uint16_t n){
	// calculate floor(log2(n))

	int leading_zeroes = 0;
	if((0xFF00 & n) == 0){
		leading_zeroes += 8;
		n <<= 8;
	}
	if((0xF000 & n) == 0){
		leading_zeroes += 4;
		n <<= 4;
	}
	if((0xC000 & n) == 0){
		leading_zeroes += 2;
		n <<= 2;
	}
	if((0x8000 & n) == 0){
		++leading_zeroes;
		n <<= 1;
	}
	if((0x8000 & n) == 0){
		++leading_zeroes;
		n <<= 1;
	}

	return 16 - leading_zeroes;
}

static uint8_t mouse_accel(uint16_t time){
	if(time < 0x2f){
		return ilog2_16(time >> 2) + 1;
	}
	else{
		return 2 * ilog2_16(time >> 3);
	}
}

void keystate_Fill_MouseReport(MouseReport_Data_t* MouseReport){
	static uint16_t mousedown_time = 1;

	// check key state
	int moving = 0;
	for(physical_keycode p_key = 0; p_key < NUM_PHYSICAL_KEYS; ++p_key){
		if(key_states[p_key].state == 1 && key_states[p_key].valid == 1){
			hid_keycode h_key = keystate_process_keycode(p_key, HID);

			if(h_key >= SPECIAL_HID_KEYS_MOUSE_START && h_key <= SPECIAL_HID_KEYS_MOUSE_END){
				switch(h_key){
				case SPECIAL_HID_KEY_MOUSE_BTN1:
					MouseReport->Button |= 1;
					break;
				case SPECIAL_HID_KEY_MOUSE_BTN2:
					MouseReport->Button |= 1<<1;
					break;
				case SPECIAL_HID_KEY_MOUSE_BTN3:
					MouseReport->Button |= 1<<2;
					break;
				case SPECIAL_HID_KEY_MOUSE_BTN4:
					MouseReport->Button |= 1<<3;
					break;
				case SPECIAL_HID_KEY_MOUSE_BTN5:
					MouseReport->Button |= 1<<4;
					break;

				case SPECIAL_HID_KEY_MOUSE_FWD:
					moving = 1;
					MouseReport->Y -= mouse_accel(mousedown_time);
					break;
				case SPECIAL_HID_KEY_MOUSE_BACK:
					moving = 1;
					MouseReport->Y += mouse_accel(mousedown_time);
					break;
				case SPECIAL_HID_KEY_MOUSE_LEFT:
					moving = 1;
					MouseReport->X -= mouse_accel(mousedown_time);
					break;
				case SPECIAL_HID_KEY_MOUSE_RIGHT:
					moving = 1;
					MouseReport->X += mouse_accel(mousedown_time);
					break;
				default:
					break;
				}
			}
		}
	}

	if(moving)
		mousedown_time++;
	else
		mousedown_time = 1;
}

hid_keycode keystate_check_hid_key(hid_keycode target_key){
	for(physical_keycode p_key = 0; p_key < NUM_PHYSICAL_KEYS; ++p_key){
		if(key_states[p_key].state == 1 && key_states[p_key].valid == 1){
			keycode h_key = keystate_process_keycode(p_key, HID);
			if(h_key == 0 || h_key == target_key){
				return h_key;
			}
		}
	}
	return NO_KEY;
}

void keystate_register_change_hook(keystate_change_hook hook){
	keystate_change_hook_fn = hook;
}
