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


#include "Keyboard.h"
#include "avr/eeprom.h"
#include <avr/interrupt.h>
#include <util/delay.h>
#include <stdarg.h>

/** Buffer to hold the previously generated Keyboard HID report, for comparison purposes inside the HID class driver. */
KeyboardReport_Data_t PrevKeyboardHIDReportBuffer;

#ifdef BUILD_FOR_LUFA
/** Buffer to hold the previously generated Mouse HID report, for comparison purposes inside the HID class driver.  Only needed for LUFA class driver. */
MouseReport_Data_t PrevMouseHIDReportBuffer;
#endif

// Keyboard
volatile uint32_t uptimems;

// keyboard settings
/* #include "4key.h" */
#include "kinesis.h"

#ifdef KEYPAD_LAYER
void KeyState_ToggleKeypad(void);
#endif

// Internet suggests that the first byte of eeprom memory is quite
// dangerous to use, as it may get clobbered in a brownout
uint8_t eeprom_safety_byte EEMEM;

// Eeprom sentinel value - if this is not set at startup, re-initialize the eeprom.
#define EEPROM_SENTINEL 42
uint8_t eeprom_sentinel_byte EEMEM;

// Persistent configuration (e.g. sound enabled)
configuration_state current_config;
configuration_state eeprom_config EEMEM;

// Key configuration is stored in eeprom. If the sentinel is not valid, initialize from the defaults.
hid_keycode logical_to_hid_map[NUM_LOGICAL_KEYS] EEMEM;

// We support saving up to 10 key mappings as their difference from the default.
#define NUM_KEY_MAPPING_INDICES 10
struct { uint8_t start; uint8_t end; } saved_key_mapping_indices[NUM_KEY_MAPPING_INDICES] EEMEM;

// Key mappings are saved as a list of (logical_keycode, hid_keycode) pairs.
#define SAVED_KEY_MAPPINGS_BUFFER_SIZE 128
struct { logical_keycode l_key; hid_keycode h_key; } saved_key_mappings[SAVED_KEY_MAPPINGS_BUFFER_SIZE] EEMEM;

// State of active keys. Keep track of all pressed or debouncing keys.
#define KEYSTATE_COUNT 14
static key_state key_states[KEYSTATE_COUNT];
static uint8_t key_press_count = 0;

// Buffer for printing.
const char MSG_NO_MACRO[] PROGMEM = "no macro support yet";
const char* print_buffer;


static state current_state = STATE_NORMAL;

// state to transition to when next action is complete:
// used for STATE_WAITING, STATE_PRINTING and STATE_EEWRITE which might transition into multiple states
static state next_state;

#ifdef KEYPAD_LAYER
static uint8_t keypad_mode;
#endif

// Predeclarations
void handle_state_normal(void);
void handle_state_programming(void);

void updateLEDs(void);
void blinkLEDs(void);

void KeyState_Init(void);
void KeyState_Update(void);

bool KeyState_CheckKey(logical_keycode key);
bool KeyState_CheckKeys(uint8_t count, ...);
void KeyState_GetKeys(logical_keycode* keys);

void Eeprom_Init(void);
void Eeprom_ResetDefaults(void);
void Eeprom_ResetFully(void);
uint8_t Eeprom_DeleteLayout(uint8_t num);
uint8_t Eeprom_SaveLayout(uint8_t num);
uint8_t Eeprom_LoadLayout(uint8_t num);

configuration_state Eeprom_LoadConfig(void);
void Eeprom_SaveConfig(configuration_state state);

#ifdef USE_BUZZER
void buzzer_init(void);
void buzzer_start(int ms);
void buzzer_update(void);
#endif

/** Main program entry point. This routine contains the overall program flow, including initial
 *  setup of all components and the main program loop.
 */
void __attribute__((noreturn)) Keyboard_Main(void)
{
	ports_init();
	KeyState_Init();
	Eeprom_Init();
#ifdef USE_BUZZER
	buzzer_init();
#endif
	current_config = Eeprom_LoadConfig();

	sei();

	struct { int keys:1; int mouse:1; } update;

	for (;;) {
		// update key state once per 2ms slice
		uint8_t slice = (uptimems & 0x1);
		if(!slice && update.keys){
			KeyState_Update();
			updateLEDs();
			update.keys = 0;
		}
		else if(!update.keys && slice){
			update.keys = 1;
		}

#ifdef USE_BUZZER
		buzzer_update();
#endif

		// in all non-wait states we want to handle the keypad layer button
#ifdef KEYPAD_LAYER
		if(current_state != STATE_WAITING && KeyState_CheckKey(LOGICAL_KEY_KEYPAD)){
			KeyState_ToggleKeypad();
			next_state = current_state;
			current_state = STATE_WAITING;
		}
#endif

		switch(current_state){
		case STATE_NORMAL:
			handle_state_normal();
			break;
		case STATE_WAITING:
			if(key_press_count == 0){
				current_state = next_state;
				next_state = 0;
			}
			break;
		case STATE_PRINTING:
			if(pgm_read_byte_near(print_buffer) == '\0'){
				current_state = STATE_WAITING;
				/* next_state = 0; */
			}
			break;
		case STATE_PROGRAMMING_SRC:
			handle_state_programming();
			break;
		case STATE_PROGRAMMING_DST:
			handle_state_programming();
			break;
		case STATE_MACRO_RECORD:
		case STATE_MACRO_PLAY:
			break;
		}

		/* Limit frequency of mouse reports. Unlike keyboard reports,
		   identical reports won't be ignored by the class driver, so
		   report speed affects mouse movement speed. */
		uint8_t mouse_slice = (uptimems & 0x8);
		uint8_t perform_mouse_update = 0;
		if(!mouse_slice && update.mouse){
			perform_mouse_update = 1;
			update.mouse = 0;
		}
		else if(!update.mouse && mouse_slice){
			update.mouse = 1;
		}

		Perform_USB_Update(1, perform_mouse_update);
	}
}

void handle_state_normal(void){
	// check for special keyboard (pre-mapping) key combinations for state transitions

	if(key_press_count >= 2 && KeyState_CheckKey(LOGICAL_KEY_PROGRAM)){

		switch(key_press_count){
		case 2:
			{
				logical_keycode keys[2];
				KeyState_GetKeys(keys);
				logical_keycode other = (keys[0] == LOGICAL_KEY_PROGRAM) ? keys[1] : keys[0];
				switch(other){
				case LOGICAL_KEY_F11:
					print_buffer = MSG_NO_MACRO;
					current_state = STATE_PRINTING;
					next_state = STATE_NORMAL;
					break;
				case LOGICAL_KEY_F12:
					current_state = STATE_WAITING;
					next_state = STATE_PROGRAMMING_SRC;
					break;
#ifdef USE_BUZZER
				case LOGICAL_KEY_BACKSLASH:
					current_config.key_sound_enabled = !current_config.key_sound_enabled;
					Eeprom_SaveConfig(current_config);
					buzzer_start(current_config.key_sound_enabled ? 300 : 150);

					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
					break;
#endif
				case LOGICAL_KEY_F7:
					Eeprom_ResetDefaults();
					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
					break;
				default:
					break;
				}
			}
			break;
		case 3:
			// full reset
			if(KeyState_CheckKeys(2, LOGICAL_KEY_F7, LOGICAL_KEY_LSHIFT)){
				Eeprom_ResetFully();
				current_state = STATE_WAITING;
				next_state = STATE_NORMAL;
			}
			else{
				// save/load/delete state : PGM + {S/L/D} + {0-9}
				logical_keycode keys[3];
				KeyState_GetKeys(keys);
				logical_keycode type = NO_KEY; // S/L/D
				logical_keycode pos = NO_KEY;  //0-9
				for(int i = 0; i < 3; ++i){
					logical_keycode ki = keys[i];
					if(ki == LOGICAL_KEY_S || ki == LOGICAL_KEY_L || ki == LOGICAL_KEY_D){
						type = ki;
					}
					else if(ki >= LOGICAL_KEY_1 && ki <= LOGICAL_KEY_0){
						pos = ki;
					}
				}
				if(type == NO_KEY || pos == NO_KEY) break;
				int index = pos - LOGICAL_KEY_1;
				int r;
				if(type == LOGICAL_KEY_S) {
					r = Eeprom_SaveLayout(index);
				} else if(type == LOGICAL_KEY_L) {
					r = Eeprom_LoadLayout(index);
				} else {
					r = Eeprom_DeleteLayout(index);
				}
				if(r){
					// show success by blinking LEDs
					blinkLEDs();
					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
				}
				else{
					// failure - we have put an error msg in print_buffer
					current_state = STATE_PRINTING;
					next_state = STATE_NORMAL;
				}
			}
			break;
		default:
			break;
		}

	}

}

void handle_state_programming(void){
	static hid_keycode program_src_hkey = 0;

	if(KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F12) ||
	  KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_HYPHEN)){
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
	}

	if(key_press_count != 1){
		return;
	}

	logical_keycode lkey;
	KeyState_GetKeys(&lkey); // Will only write one key, as key_press_count == 1

	hid_keycode default_hkey = pgm_read_byte_near(&logical_to_hid_map_default[lkey]);

	// can't reprogram a "special" key type (i.e program, keypad), but NO_KEY is ok.
	if(default_hkey >= SPECIAL_HID_KEYS_NOREMAP_START && default_hkey != NO_KEY){
		return;
	}

	if(current_state == STATE_PROGRAMMING_SRC){
		program_src_hkey = default_hkey;
		next_state = STATE_PROGRAMMING_DST;
		current_state = STATE_WAITING;
	}
	else{
		// ok, copy the saved default hkey for the src lkey to the dst lkey.
		eeprom_update_byte(&logical_to_hid_map[lkey], program_src_hkey);
		current_state = STATE_WAITING;
		next_state = STATE_PROGRAMMING_SRC;
	}
}



/**
 * Looks up the key data (config/defaults) index associated with a
 * matrix position.  Not all matrix positions will have a key
 * associated.
 */
logical_keycode getMatrixIndex(matrix_keycode matrixKey){
	return pgm_read_byte_near(&matrix_to_logical_map[matrixKey / MATRIX_COLS][matrixKey % MATRIX_COLS]);
}

void KeyState_Init(void){
	for(uint8_t i = 0 ; i < KEYSTATE_COUNT; ++i){
		key_states[i].l_key    = NO_KEY;
		key_states[i].state    = 0;
		key_states[i].debounce = 0;
	}
}


void KeyState_Update(void){
	// for each entry i in the matrix
	for(uint8_t matrix_row = 0; matrix_row < MATRIX_ROWS; ++matrix_row){
		matrix_select_row(matrix_row);
		for(uint8_t matrix_col = 0; matrix_col < MATRIX_COLS; ++matrix_col){

			// look up the logical key for the matrix code
			logical_keycode l_key =  pgm_read_byte_near(&matrix_to_logical_map[matrix_row][matrix_col]);
			if(l_key == NO_KEY) goto next_matrix; // empty space in the sparse matrix

#ifdef KEYPAD_LAYER // keyboard uses a "keypad layer" - duplicate mappings for many of its keys
			if(keypad_mode && l_key >= KEYPAD_LAYER_START){
				l_key += KEYPAD_LAYER_SIZE;
			}
#endif
			uint8_t reading = matrix_read_column(matrix_col);

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
						if(key->state) key_press_count--;
						key->l_key = NO_KEY;
						key->state = 0;
					}
					else{
						if(key->state == 0 && key->debounce == DEBOUNCE_MASK){
							++key_press_count;
							key->state = 1;
							#ifdef USE_BUZZER
							if(current_config.key_sound_enabled)
								buzzer_start(1);
							#endif
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


#ifdef KEYPAD_LAYER
void KeyState_ToggleKeypad(void){
	keypad_mode = !keypad_mode;
	// And clear all currently pressed keys that are now no longer available
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		logical_keycode l_key = key_states[i].l_key;

		// if the key is valid in the new mode, continue
		if(l_key < KEYPAD_LAYER_START) continue;
		if(keypad_mode){
			if(l_key >= (KEYPAD_LAYER_START + KEYPAD_LAYER_SIZE)) continue; // safe
		}
		else{
			if(l_key < (KEYPAD_LAYER_START + KEYPAD_LAYER_SIZE)) continue;
		}

		// otherwise clear the key state
		key_states[i].l_key = NO_KEY;
		if(key_states[i].state){
			--key_press_count;
		}
		key_states[i].state = 0;
	}
}
#endif


bool KeyState_CheckKey(logical_keycode l_key){
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		if(key_states[i].l_key == l_key){
			return key_states[i].state;
		}
	}
	return false;
}

// returns true if all argument keys are down
bool KeyState_CheckKeys(uint8_t count, ...){
	if(count > key_press_count) return false; // trivially know it's impossible

	va_list argp;
	bool success = true;
	va_start(argp, count);
	while(count--){
		logical_keycode lkey = va_arg(argp, int);
		bool found_key = KeyState_CheckKey(lkey);

		if(!found_key){
			success = false;
			break;
		}
	}

	va_end(argp);
	return success;
}

/* writes up to key_press_count currently pressed key indexes to the
   output buffer keys */
void KeyState_GetKeys(logical_keycode* l_keys){
	int ki = 0;
	for(int i = 0; i < KEYSTATE_COUNT && ki < key_press_count; ++i){
		if(key_states[i].state){
			l_keys[ki++] = key_states[i].l_key;
		}
	}
}


// blink the LEDs for a while to show that we did some work successfully
void blinkLEDs(void){
	for(int i = 0; i < 5; ++i){
		set_all_leds(LEDMASK_SCROLLLOCK | LEDMASK_CAPS);
		_delay_ms(50);
		set_all_leds(LEDMASK_SCROLLLOCK | LEDMASK_NUMLOCK);
		_delay_ms(50);
	}

}

// reset the current layout to the default layout
void Eeprom_ResetDefaults(void){
	for(int i = 0; i < NUM_LOGICAL_KEYS; ++i){
		hid_keycode default_key = pgm_read_byte_near(&logical_to_hid_map_default[i]);
		eeprom_update_byte(&logical_to_hid_map[i], default_key);
	}
	eeprom_update_byte((uint8_t*)&eeprom_config, 0x0);
	// flash LEDs to show that we had to reset
	blinkLEDs();
}

// reset the keyboard, including saved layouts
void Eeprom_ResetFully(void){
	eeprom_update_byte(&eeprom_sentinel_byte, EEPROM_SENTINEL);
	for(int i = 0; i < NUM_KEY_MAPPING_INDICES; ++i){
		eeprom_update_byte(&saved_key_mapping_indices[i].start, NO_KEY);
	}
	Eeprom_ResetDefaults();
}

configuration_state Eeprom_LoadConfig(void){
	union {
		uint8_t b;
		configuration_state s;
	} r;
	r.b = eeprom_read_byte((uint8_t*)&eeprom_config);
	return r.s;
}

void Eeprom_SaveConfig(configuration_state state){
	union {
		uint8_t b;
		configuration_state s;
	} r;
	r.s = state;
	eeprom_update_byte((uint8_t*)&eeprom_config, r.b);
}


static const char MSG_NO_LAYOUT[] PROGMEM = "No such layout";

uint8_t Eeprom_DeleteLayout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		print_buffer = MSG_NO_LAYOUT;
		return false;
	}
	uint8_t start = eeprom_read_byte(&saved_key_mapping_indices[num].start);
	if(start == NO_KEY){
		print_buffer = MSG_NO_LAYOUT;
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



uint8_t Eeprom_SaveLayout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		print_buffer = MSG_NO_LAYOUT;
		return false;
	}

	// remove old layout
	Eeprom_DeleteLayout(num);

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
				print_buffer = msg;
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
		print_buffer = msg;
		return false;
	}
}

uint8_t Eeprom_LoadLayout(uint8_t num){
	if(num >= NUM_KEY_MAPPING_INDICES){
		print_buffer = MSG_NO_LAYOUT;
		return false;
	}

	uint8_t start = eeprom_read_byte(&saved_key_mapping_indices[num].start);
	if(start == NO_KEY){
		print_buffer = MSG_NO_LAYOUT;
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

void Eeprom_Init(void){
	uint8_t sentinel = eeprom_read_byte(&eeprom_sentinel_byte);
	if(sentinel != EEPROM_SENTINEL){
		Eeprom_ResetFully();
	}
}

static void Fill_HIDReport_normal(KeyboardReport_Data_t* KeyboardReport){
	uint8_t UsedKeyCodes = 0;
	uint8_t rollover = false;
	// todo: macro mode: if i'm in macro mode, ignore my state and fire the next events in the macro

	// check key state
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		if(key_states[i].state){
			if(UsedKeyCodes == 6){
				rollover = true;
				break;
			}
			logical_keycode l_key = key_states[i].l_key;
			/* hid_keycode h_key = pgm_read_byte_near(&logical_to_hid_map_default[l_key]); */
			hid_keycode h_key = eeprom_read_byte(&logical_to_hid_map[l_key]); // Disable programmability for the moment

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
		for(int i = 0; i < 6; ++i)
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

bool Fill_MouseReport(MouseReport_Data_t* MouseReport){
	static uint16_t mousedown_time = 1;

	static uint8_t last_button_report = 0;

	// check key state
	int send = 0;
	int moving = 0;
	for(int i = 0; i < KEYSTATE_COUNT; ++i){
		if(key_states[i].state){
			logical_keycode l_key = key_states[i].l_key;
			hid_keycode h_key = eeprom_read_byte(&logical_to_hid_map[l_key]); // Disable programmability for the moment
			if(h_key >= SPECIAL_HID_KEYS_MOUSE_START && h_key <= SPECIAL_HID_KEYS_MOUSE_END){
				send = 1;
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

	if(MouseReport->Button != last_button_report) send = true; // If the buttons have changed, send a report immediately
	last_button_report = MouseReport->Button;

	return send;
}

static void char_to_keys(const char nextchar, hid_keycode* nextkey, hid_keycode* nextmod){
	*nextkey = 0;
	*nextmod = 0;

	// letters:
	uint8_t l = nextchar | 0x20;
	if('a' <= l && 'z' >= l){
		*nextkey = HID_KEYBOARD_SC_A + (l - 'a');
		if(!(nextchar & 0x20)) *nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
	}
	else{
		switch(nextchar){
		case ' ':
			*nextkey = HID_KEYBOARD_SC_SPACE;
			break;
		case '!':
			*nextkey = HID_KEYBOARD_SC_1_AND_EXCLAMATION;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '"':
			*nextkey = HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '#':
			*nextkey = HID_KEYBOARD_SC_3_AND_HASHMARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '$':
			*nextkey = HID_KEYBOARD_SC_4_AND_DOLLAR;
			*nextmod =  HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '%':
			*nextkey = HID_KEYBOARD_SC_5_AND_PERCENTAGE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '&':
			*nextkey = HID_KEYBOARD_SC_7_AND_AND_AMPERSAND;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '\'':
			*nextkey = HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE;
			break;
		case '(':
			*nextkey = HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case ')':
			*nextkey = HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '*':
			*nextkey = HID_KEYBOARD_SC_8_AND_ASTERISK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '+':
			*nextkey = HID_KEYBOARD_SC_EQUAL_AND_PLUS;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case ',':
			*nextkey = HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN;
			break;
		case '-':
			*nextkey = HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE;
			break;
		case '.':
			*nextkey = HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN;
			break;
		case '/':
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			break;
		case '0':
			*nextkey = HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS;
			break;
		case '1':
			*nextkey = HID_KEYBOARD_SC_1_AND_EXCLAMATION;
			break;
		case '2':
			*nextkey = HID_KEYBOARD_SC_2_AND_AT;
			break;
		case '3':
			*nextkey = HID_KEYBOARD_SC_3_AND_HASHMARK;
			break;
		case '4':
			*nextkey = HID_KEYBOARD_SC_4_AND_DOLLAR;
			break;
		case '5':
			*nextkey = HID_KEYBOARD_SC_5_AND_PERCENTAGE;
			break;
		case '6':
			*nextkey = HID_KEYBOARD_SC_6_AND_CARET;
			break;
		case '7':
			*nextkey = HID_KEYBOARD_SC_7_AND_AND_AMPERSAND;
			break;
		case '8':
			*nextkey = HID_KEYBOARD_SC_8_AND_ASTERISK;
			break;
		case '9':
			*nextkey = HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS;
			break;
		case ':':
			*nextkey = HID_KEYBOARD_SC_SEMICOLON_AND_COLON;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case ';':
			*nextkey = HID_KEYBOARD_SC_SEMICOLON_AND_COLON;
			break;
		case '<':
			*nextkey = HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '=':
			*nextkey = HID_KEYBOARD_SC_EQUAL_AND_PLUS;
			break;
		case '>':
			*nextkey = HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '?':
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '@':
			*nextkey = HID_KEYBOARD_SC_2_AND_AT;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '[':
			*nextkey = HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE;
			break;
		case '\\':
			*nextkey = HID_KEYBOARD_SC_BACKSLASH_AND_PIPE;
			break;
		case ']':
			*nextkey = HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE;
			break;
		case '^':
			*nextkey = HID_KEYBOARD_SC_6_AND_CARET;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '_':
			*nextkey = HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '`':
			*nextkey = HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '{':
			*nextkey = HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '|':
			*nextkey = HID_KEYBOARD_SC_BACKSLASH_AND_PIPE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '}':
			*nextkey = HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '~':
			*nextkey = HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE;
			break;
		default:
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		}
	}
}

void Fill_HIDReport_printing(KeyboardReport_Data_t* ReportData){
	// if the last report was a key, send empty. Otherwise send the
	// next character from print_buffer
	if(PrevKeyboardHIDReportBuffer.Modifier || PrevKeyboardHIDReportBuffer.KeyCode[0]){
		return; // empty report
	}
	else{
		char nextchar = pgm_read_byte_near(print_buffer++);
		uint8_t key, mod;
		char_to_keys(nextchar, &key, &mod);
		ReportData->Modifier = mod;
		ReportData->KeyCode[0] = key; //HID_KEYBOARD_SC_A + counter; // key;
		//		counter = (counter + 1) % 26;
	}
}

/**
 * Fills the argument buffer with a keyboard report according to the
 * current state returns true if the report must be sent, false if it
 * may be compared to the previous report before sending.
 */
bool Fill_KeyboardReport(KeyboardReport_Data_t* KeyboardReport){
	switch(current_state){
	case STATE_NORMAL:
	case STATE_PROGRAMMING_SRC:
	case STATE_PROGRAMMING_DST:
		Fill_HIDReport_normal(KeyboardReport);
		return false;
	case STATE_PRINTING:
		Fill_HIDReport_printing(KeyboardReport);
		return true;
	case STATE_MACRO_RECORD:
		Fill_HIDReport_normal(KeyboardReport);
		// TODO: If this report is different to the previous one, save it in the macro buffer.
	case STATE_MACRO_PLAY:
		// TODO: Fetch the next report from the macro buffer and replay it
	default:
		// We're not in a state which allows typing: report no keys
		return false;
	}
}


// LEDs
static uint8_t USB_LEDReport = 0;

void updateLEDs(void){
	uint8_t  LEDMask = 0;

#ifdef KEYPAD_LAYER
		if(keypad_mode)
			LEDMask |= LEDMASK_KEYPAD;
#endif

	switch(current_state){
	case STATE_PROGRAMMING_SRC:
		// flash quickly - change every 128ms
		if(uptimems & 128){
			LEDMask |= LEDMASK_PROGRAMMING_SRC;
		}
		break;
	case STATE_PROGRAMMING_DST:
		// flash slowly - change every 256ms
		if(uptimems & 256){
			LEDMask |= LEDMASK_PROGRAMMING_DST;
		}
		break;
	case STATE_NORMAL:
	default:
		// populate from USB LED report
		if (USB_LEDReport & HID_KEYBOARD_LED_NUMLOCK)
			LEDMask |= LEDMASK_NUMLOCK;

		if (USB_LEDReport & HID_KEYBOARD_LED_CAPSLOCK)
			LEDMask |= LEDMASK_CAPS;

		if (USB_LEDReport & HID_KEYBOARD_LED_SCROLLLOCK)
			LEDMask |= LEDMASK_SCROLLLOCK;


		break;
	}

	set_all_leds(LEDMask);
}

void Process_KeyboardLEDReport(uint8_t report){
	USB_LEDReport = report;
}

void Update_USBState(USB_State state){
	switch(state){
	case NOTREADY:
		set_all_leds(LEDMASK_USB_NOTREADY);
		break;
	case ENUMERATING:
		set_all_leds(LEDMASK_USB_ENUMERATING);
		break;
	case READY:
		set_all_leds(LEDMASK_USB_READY);
		break;
	case ERROR:
		set_all_leds(LEDMASK_USB_ERROR);
		break;
	}
}

void Update_Millis(uint8_t increment){
	uptimems += increment;
}

#ifdef USE_BUZZER
static uint16_t buzzer_ms;

static const int TIMER_FREQ = ((1<<CS01) | (1<<CS00));

void buzzer_init(void){
	// up timer0 for CTC mode at FCPU / 64: 4us per tick, enable compare interrupt
	TCNT0 = 0;
	OCR0  = 150;
	TCCR0 |= (1<<WGM01);
	TIMSK |= (1<<OCIE0);
}

void buzzer_start(int ms){
	int end = uptimems + ms;
	if(buzzer_ms < end){
		buzzer_ms = end;

		// Turn on the buzzer and start the timer
		BUZZER_PORT |= BUZZER;
		TCCR0       |= TIMER_FREQ;
	}
}

void buzzer_update(void){
	if(buzzer_ms && buzzer_ms <= uptimems){
		buzzer_ms = 0;
		// Stop the timer and turn off the buzzer
		TCCR0       &= ~TIMER_FREQ;
		BUZZER_PORT &= ~BUZZER;
	}
}

ISR(TIMER0_COMP_vect){
	BUZZER_PORT ^= BUZZER;
}

#endif
