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
static key_state key_states[KEYSTATE_COUNT];

static keystate_change_hook keystate_change_hook_fn = 0;

uint8_t key_press_count = 0;

struct {
	unsigned char toggle:1;
	unsigned char shift_count:7;
} keypad_state;

void keystate_init(void){
	for(uint8_t i = 0 ; i < KEYSTATE_COUNT; ++i){
		key_states[i].l_key    = NO_KEY;
		key_states[i].state    = 0;
		key_states[i].debounce = 0;
	}
}

bool keystate_is_keypad_mode(void){
	return (keypad_state.toggle != 0) || (keypad_state.shift_count != 0);
}

static inline void keystate_set_key(key_state* key){
	++key_press_count;
	key->state = 1;
	if(keystate_change_hook_fn) keystate_change_hook_fn(key->l_key, true);
	#if USE_BUZZER
	if(config_get_flags().key_sound_enabled)
		buzzer_start(3);
	#endif
}

static inline uint8_t keystate_clear_key(key_state* key){
	uint8_t old_state = key->state;
	logical_keycode old_key = key->l_key;

	key->l_key = NO_KEY;
	key->state = 0;

	if(old_state){ // if it had been pressed
		key_press_count--;
		if(keystate_change_hook_fn) keystate_change_hook_fn(old_key, false);
	}

	return old_state;
}

// Called when a keypad key (either toggle or shift) changes state. If the key
// change causes the keypad mode to be toggled, all currently tracked keys that
// are no longer valid in the new mode are reset. Returns true if the keypad
// state changed.
static uint8_t keystate_update_keypad(hid_keycode keypad_key, uint8_t state){
	uint8_t prev_keypad_mode = keystate_is_keypad_mode();

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

	// Otherwise, clear all currently tracked keys that are now no longer available
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		logical_keycode l_key = key_states[i].l_key;
		hid_keycode h_key = config_get_definition(l_key);

		// if the tracked key is valid in the new mode, continue
		if(SPECIAL_HID_KEY_NOREMAP(h_key)){
			continue;
		}
		else if(keypad_mode){
			if(l_key >= KEYPAD_LAYER_SIZE) continue; // safe
		}
		else{
			if(l_key < KEYPAD_LAYER_SIZE) continue;
		}

		// otherwise clear the key state
		keystate_clear_key(&key_states[i]);
	}
	return true;
}



void keystate_update(void){
	// for each entry i in the matrix
	for(uint8_t matrix_row = 0; matrix_row < MATRIX_ROWS; ++matrix_row){
		matrix_select_row(matrix_row);

		for(uint8_t matrix_col = 0; matrix_col < MATRIX_COLS; ++matrix_col){
			uint8_t reading = matrix_read_column(matrix_col);

			// look up the logical key for the matrix code
			// Note that only one matrix position should map to any given
			// logical code: otherwise we won't register a keypress unless both
			// are pressed: one position will be debouncing up and the other
			// down.
			logical_keycode l_key = storage_read_byte(CONSTANT_STORAGE, &matrix_to_logical_map[matrix_row][matrix_col]);
			if(l_key == NO_KEY) goto next_matrix; // empty space in the sparse matrix

			hid_keycode h_key = config_get_definition(l_key);
			bool noremap_key = SPECIAL_HID_KEY_NOREMAP(h_key);

			// Handle layer switch. No-remap (keypad and program) keys are ignored.
			if(keystate_is_keypad_mode() && !noremap_key){
				l_key += KEYPAD_LAYER_SIZE;
			}

			uint8_t free_slot = NO_KEY;

			// Scan the current keystates. If we find an entry for our key, update it.
			// if we don't, and the key is pressed, add it to a free slot.
			for(uint8_t j = 0; j < KEYSTATE_COUNT; ++j){
				key_state* key = &key_states[j];
				if(free_slot == NO_KEY && key->l_key == NO_KEY){ // found a free slot
					free_slot = j;
				}
				else if(key->l_key == l_key){ //found our key
					// update the debounce mask with the current reading
					key->debounce = DEBOUNCE_MASK & ((key->debounce << 1) | reading);

					if(key->debounce == 0x00){
						// key is not pressed (either debounced-down or never made it up), remove it
						uint8_t old_state = keystate_clear_key(key);
						if(old_state && noremap_key){
							uint8_t changed = keystate_update_keypad(h_key, false);
							if(changed){ matrix_col = 0; matrix_row = 0; }
						}
					}
					else{
						if(key->state == 0 && key->debounce == DEBOUNCE_MASK){
							keystate_set_key(key);
							if(noremap_key){
								uint8_t changed = keystate_update_keypad(h_key, true);
								if(changed){ matrix_col = 0; matrix_row = 0; }
							}
						}
					}
					goto next_matrix; // done with this reading
				}
			}
			// key was not in the state, so previously not pressed.
			// If pressed now, record a new key if there's space.
			if(reading && free_slot != NO_KEY){
				key_state* key = &key_states[free_slot];
				key->l_key = l_key;
				key->state = 0;
				key->debounce = 0x1;
			}
		next_matrix:;
		}
	}
}

static inline keycode keystate_process_keycode(logical_keycode raw_key, keycode_type ktype){
	keycode key = raw_key;
	switch(ktype){
	case PHYSICAL:
		if(key >= KEYPAD_LAYER_SIZE){ key -= KEYPAD_LAYER_SIZE; }
		break;
	case HID:
		key = config_get_definition(key);
		break;
	case LOGICAL:
		break;
	}
	return key;
}

bool keystate_check_key(keycode target_key, keycode_type ktype){
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		logical_keycode raw_key = key_states[i].l_key;
		keycode key = keystate_process_keycode(raw_key, ktype);

		if(key == target_key){
			return key_states[i].state;
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
	for(int i = 0; i < KEYSTATE_COUNT && ki < key_press_count; ++i){
		if(key_states[i].state){
			logical_keycode raw_key = key_states[i].l_key;
			keycode key = keystate_process_keycode(raw_key, ktype);

			keys[ki++] = key;
		}
	}
}

void keystate_Fill_KeyboardReport(KeyboardReport_Data_t* KeyboardReport){
	uint8_t UsedKeyCodes = 0;
	uint8_t rollover = false;
	// check key state
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		if(key_states[i].state){
			if(UsedKeyCodes == KEYBOARDREPORT_KEY_COUNT){
				rollover = true;
				break;
			}
			logical_keycode l_key = key_states[i].l_key;
			hid_keycode h_key = config_get_definition(l_key);

			if(h_key == SPECIAL_HID_KEY_PROGRAM) rollover = true; // Simple way to ensure program key combinations never cause typing

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
				KeyboardReport->KeyCode[UsedKeyCodes++] = h_key;
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
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		if(key_states[i].state){
			logical_keycode l_key = key_states[i].l_key;
			hid_keycode h_key = config_get_definition(l_key);
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

hid_keycode keystate_check_hid_key(hid_keycode key){
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		if(key_states[i].state){
			logical_keycode l_key = key_states[i].l_key;
			hid_keycode h_key = config_get_definition(l_key);
			if(key == 0 || key == h_key) return h_key;
		}
	}
	return 0xFF;
}

/**
 * writes up to key_press_count currently pressed HID keycodes to the
 * output buffer keys. If exclude_special is set, do not write any
 * special keycodes. Returns number of keycodes written.
 */
int keystate_get_hid_keys(hid_keycode* h_keys, bool exclude_special){
	int ki = 0;
	for(int i = 0; i < KEYSTATE_COUNT && ki < key_press_count; ++i){
		if(key_states[i].state){
			logical_keycode l_key = key_states[i].l_key;
			hid_keycode h_key = config_get_definition(l_key);
			if(exclude_special && h_key >= SPECIAL_HID_KEYS_START){
				continue;
			}
			h_keys[ki++] = h_key;
		}
	}
	return ki;
}


void keystate_register_change_hook(keystate_change_hook hook){
	keystate_change_hook_fn = hook;
}
