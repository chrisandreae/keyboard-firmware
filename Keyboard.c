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

#include "avr/eeprom.h"
#include <avr/interrupt.h>
#include <util/delay.h>
#include <stdarg.h>

#include "Keyboard.h"
#include "twi.h"

// keyboard settings
/* #include "4key.h" */
#include "kinesis.h"

/** Buffer to hold the previously generated Keyboard HID report, for comparison purposes inside the HID class driver. */
KeyboardReport_Data_t PrevKeyboardHIDReportBuffer;

#ifdef BUILD_FOR_LUFA
/** Buffer to hold the previously generated Mouse HID report, for comparison purposes inside the HID class driver.  Only needed for LUFA class driver. */
MouseReport_Data_t PrevMouseHIDReportBuffer;
#endif

// Keyboard
volatile uint32_t uptimems;


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
void buzzer_start(uint16_t ms);
void buzzer_update(uint8_t increment);
#endif

#ifdef USE_EEPROM
uint8_t serial_eeprom_write_byte(uint16_t addr, uint8_t data);
int16_t serial_eeprom_write(uint16_t addr, uint8_t* buf, int16_t len);
int16_t serial_eeprom_read(uint16_t addr, uint8_t* buf, int16_t len);
uint8_t serial_eeprom_test_read(void);
#endif

/** Main program entry point. This routine contains the overall program flow, including initial
 *  setup of all components and the main program loop.
 */
void __attribute__((noreturn)) Keyboard_Main(void)
{
	ports_init();
	KeyState_Init();
	Eeprom_Init();
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
					buzzer_start(current_config.key_sound_enabled ? 150 : 75);

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

/* Serial eeprom support */

static const uint8_t SERIAL_EEPROM_WRITE_TIME_MS = 10;

static enum { SUCCESS, WSELECT_ERROR, RSELECT_ERROR, ADDRESS_ERROR, DATA_ERROR } serial_eeprom_errno = SUCCESS;

// communicate with AT24C164 serial eeprom(s)
uint8_t serial_eeprom_write_byte(uint16_t addr, uint8_t data){
	serial_eeprom_errno = SUCCESS;

	twi_start();

	// [ 1 | A2 | A1 | A0 | B2 | B1 | B0 | R/W ] A0-2 = device address, B0-2 = 3 MSB of 11-bit device address
	uint8_t address_byte = 0b10100000; // 010 address (all low) and write operation
	address_byte ^= ((addr >> 7) & 0b01111110); // select 14-bit device-and-address at once

	if(twi_write_byte(address_byte) != ACK){
		serial_eeprom_errno = WSELECT_ERROR; goto fail;
	}
	if(twi_write_byte(addr & 0xff) != ACK){
		serial_eeprom_errno = ADDRESS_ERROR; goto fail;
	}
	if(twi_write_byte(data) != ACK){
		serial_eeprom_errno = DATA_ERROR; goto fail;
	}

 fail:
	twi_stop();
	return serial_eeprom_errno == SUCCESS;
}

int16_t serial_eeprom_write(uint16_t addr, uint8_t* buf, int16_t len){
	// todo: support page write to save rewrite cycles
	serial_eeprom_errno = SUCCESS;

	for(int i = 0; i < len; ++i){
		if(!serial_eeprom_write_byte(addr+i, buf[i])){
			if(i) return i;
			else return -1;
		}
	}
	return len;
}

int16_t serial_eeprom_read(uint16_t addr, uint8_t* buf, int16_t len){
	serial_eeprom_errno = SUCCESS;
	uint8_t read_bytes = 0;

	twi_start();

	// Set the current address by doing a "dummy write" to the address -
	// set up as though writing, but then don't send the actual byte

	// [ 1 | A2 | A1 | A0 | B2 | B1 | B0 | R/W ] A0-2 = device address, B0-2 = 3 MSB of 11-bit device address
	uint8_t address_byte = 0b10100000; // 010 address (all low) and write operation
	address_byte ^= ((addr >> 7) & 0b01111110); // select 14-bit device-and-address at once

	if(twi_write_byte(address_byte) != ACK){
		serial_eeprom_errno = WSELECT_ERROR; goto fail;
	}
	if(twi_write_byte(addr & 0xff) != ACK){
		serial_eeprom_errno = ADDRESS_ERROR; goto fail;
	}

	// then send a start again, and the address with the read bit set.
	twi_start();

	uint8_t read_addr = address_byte | 0x1; // do I need to clip out the high bits here?
	if(twi_write_byte(read_addr) != ACK){
		serial_eeprom_errno = RSELECT_ERROR; goto fail;
	}

	while(len--){
		*buf++ = twi_read_byte(len ? ACK : NACK); // nack on last byte to stop it talking to us
		++read_bytes;
	}

 fail:
	twi_stop();
	return read_bytes ? read_bytes : -1;
}


static const char* const print_byte(const uint8_t byte);

uint8_t serial_eeprom_test_read(void){
	static uint16_t addr = 0;
	uint8_t b;

	int16_t r = serial_eeprom_read(addr++, &b, 1);
	if(r != 1){
		switch(serial_eeprom_errno){
		case RSELECT_ERROR:{
			static const char msg[] PROGMEM = "RSELECT_ERROR";
			print_buffer = msg;
			break;
		}
		case WSELECT_ERROR:{
			static const char msg[] PROGMEM = "WSELECT_ERROR";
			print_buffer = msg;
			break;
		}
		case ADDRESS_ERROR:{
			static const char msg[] PROGMEM = "ADDRESS_ERROR";
			print_buffer = msg;
			break;
		}
		case DATA_ERROR:{
			static const char msg[] PROGMEM = "DATA_ERROR";
			print_buffer = msg;
			break;
		}
		default:{
			static const char msg[] PROGMEM = "WTF_ERROR";
			print_buffer = msg;
		}
		}
		return 0;
	}
	else{
		print_buffer = print_byte(b);
		return 1;
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
			if(l_key == LOGICAL_KEY_PROGRAM) rollover = true; // Simple way to ensure program key combinations never cause typing

			hid_keycode h_key = eeprom_read_byte(&logical_to_hid_map[l_key]);

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
		case '\n':
			*nextkey = HID_KEYBOARD_SC_ENTER;
			break;
		case '\t':
			*nextkey = HID_KEYBOARD_SC_TAB;
			break;
		default:
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		}
	}
}

static const char* const print_byte(const uint8_t byte){
	switch(byte){
	case 1:   { static const char msg[] PROGMEM = "01 "; return msg; }
	case 2:   { static const char msg[] PROGMEM = "02 "; return msg; }
	case 3:   { static const char msg[] PROGMEM = "03 "; return msg; }
	case 4:   { static const char msg[] PROGMEM = "04 "; return msg; }
	case 5:   { static const char msg[] PROGMEM = "05 "; return msg; }
	case 6:   { static const char msg[] PROGMEM = "06 "; return msg; }
	case 7:   { static const char msg[] PROGMEM = "07 "; return msg; }
	case 8:   { static const char msg[] PROGMEM = "08 "; return msg; }
	case 9:   { static const char msg[] PROGMEM = "09 "; return msg; }
	case 10:  { static const char msg[] PROGMEM = "0A "; return msg; }
	case 11:  { static const char msg[] PROGMEM = "0B "; return msg; }
	case 12:  { static const char msg[] PROGMEM = "0C "; return msg; }
	case 13:  { static const char msg[] PROGMEM = "0D "; return msg; }
	case 14:  { static const char msg[] PROGMEM = "0E "; return msg; }
	case 15:  { static const char msg[] PROGMEM = "0F "; return msg; }
	case 16:  { static const char msg[] PROGMEM = "10 "; return msg; }
	case 17:  { static const char msg[] PROGMEM = "11 "; return msg; }
	case 18:  { static const char msg[] PROGMEM = "12 "; return msg; }
	case 19:  { static const char msg[] PROGMEM = "13 "; return msg; }
	case 20:  { static const char msg[] PROGMEM = "14 "; return msg; }
	case 21:  { static const char msg[] PROGMEM = "15 "; return msg; }
	case 22:  { static const char msg[] PROGMEM = "16 "; return msg; }
	case 23:  { static const char msg[] PROGMEM = "17 "; return msg; }
	case 24:  { static const char msg[] PROGMEM = "18 "; return msg; }
	case 25:  { static const char msg[] PROGMEM = "19 "; return msg; }
	case 26:  { static const char msg[] PROGMEM = "1A "; return msg; }
	case 27:  { static const char msg[] PROGMEM = "1B "; return msg; }
	case 28:  { static const char msg[] PROGMEM = "1C "; return msg; }
	case 29:  { static const char msg[] PROGMEM = "1D "; return msg; }
	case 30:  { static const char msg[] PROGMEM = "1E "; return msg; }
	case 31:  { static const char msg[] PROGMEM = "1F "; return msg; }
	case 32:  { static const char msg[] PROGMEM = "20 "; return msg; }
	case 33:  { static const char msg[] PROGMEM = "21 "; return msg; }
	case 34:  { static const char msg[] PROGMEM = "22 "; return msg; }
	case 35:  { static const char msg[] PROGMEM = "23 "; return msg; }
	case 36:  { static const char msg[] PROGMEM = "24 "; return msg; }
	case 37:  { static const char msg[] PROGMEM = "25 "; return msg; }
	case 38:  { static const char msg[] PROGMEM = "26 "; return msg; }
	case 39:  { static const char msg[] PROGMEM = "27 "; return msg; }
	case 40:  { static const char msg[] PROGMEM = "28 "; return msg; }
	case 41:  { static const char msg[] PROGMEM = "29 "; return msg; }
	case 42:  { static const char msg[] PROGMEM = "2A "; return msg; }
	case 43:  { static const char msg[] PROGMEM = "2B "; return msg; }
	case 44:  { static const char msg[] PROGMEM = "2C "; return msg; }
	case 45:  { static const char msg[] PROGMEM = "2D "; return msg; }
	case 46:  { static const char msg[] PROGMEM = "2E "; return msg; }
	case 47:  { static const char msg[] PROGMEM = "2F "; return msg; }
	case 48:  { static const char msg[] PROGMEM = "30 "; return msg; }
	case 49:  { static const char msg[] PROGMEM = "31 "; return msg; }
	case 50:  { static const char msg[] PROGMEM = "32 "; return msg; }
	case 51:  { static const char msg[] PROGMEM = "33 "; return msg; }
	case 52:  { static const char msg[] PROGMEM = "34 "; return msg; }
	case 53:  { static const char msg[] PROGMEM = "35 "; return msg; }
	case 54:  { static const char msg[] PROGMEM = "36 "; return msg; }
	case 55:  { static const char msg[] PROGMEM = "37 "; return msg; }
	case 56:  { static const char msg[] PROGMEM = "38 "; return msg; }
	case 57:  { static const char msg[] PROGMEM = "39 "; return msg; }
	case 58:  { static const char msg[] PROGMEM = "3A "; return msg; }
	case 59:  { static const char msg[] PROGMEM = "3B "; return msg; }
	case 60:  { static const char msg[] PROGMEM = "3C "; return msg; }
	case 61:  { static const char msg[] PROGMEM = "3D "; return msg; }
	case 62:  { static const char msg[] PROGMEM = "3E "; return msg; }
	case 63:  { static const char msg[] PROGMEM = "3F "; return msg; }
	case 64:  { static const char msg[] PROGMEM = "40 "; return msg; }
	case 65:  { static const char msg[] PROGMEM = "41 "; return msg; }
	case 66:  { static const char msg[] PROGMEM = "42 "; return msg; }
	case 67:  { static const char msg[] PROGMEM = "43 "; return msg; }
	case 68:  { static const char msg[] PROGMEM = "44 "; return msg; }
	case 69:  { static const char msg[] PROGMEM = "45 "; return msg; }
	case 70:  { static const char msg[] PROGMEM = "46 "; return msg; }
	case 71:  { static const char msg[] PROGMEM = "47 "; return msg; }
	case 72:  { static const char msg[] PROGMEM = "48 "; return msg; }
	case 73:  { static const char msg[] PROGMEM = "49 "; return msg; }
	case 74:  { static const char msg[] PROGMEM = "4A "; return msg; }
	case 75:  { static const char msg[] PROGMEM = "4B "; return msg; }
	case 76:  { static const char msg[] PROGMEM = "4C "; return msg; }
	case 77:  { static const char msg[] PROGMEM = "4D "; return msg; }
	case 78:  { static const char msg[] PROGMEM = "4E "; return msg; }
	case 79:  { static const char msg[] PROGMEM = "4F "; return msg; }
	case 80:  { static const char msg[] PROGMEM = "50 "; return msg; }
	case 81:  { static const char msg[] PROGMEM = "51 "; return msg; }
	case 82:  { static const char msg[] PROGMEM = "52 "; return msg; }
	case 83:  { static const char msg[] PROGMEM = "53 "; return msg; }
	case 84:  { static const char msg[] PROGMEM = "54 "; return msg; }
	case 85:  { static const char msg[] PROGMEM = "55 "; return msg; }
	case 86:  { static const char msg[] PROGMEM = "56 "; return msg; }
	case 87:  { static const char msg[] PROGMEM = "57 "; return msg; }
	case 88:  { static const char msg[] PROGMEM = "58 "; return msg; }
	case 89:  { static const char msg[] PROGMEM = "59 "; return msg; }
	case 90:  { static const char msg[] PROGMEM = "5A "; return msg; }
	case 91:  { static const char msg[] PROGMEM = "5B "; return msg; }
	case 92:  { static const char msg[] PROGMEM = "5C "; return msg; }
	case 93:  { static const char msg[] PROGMEM = "5D "; return msg; }
	case 94:  { static const char msg[] PROGMEM = "5E "; return msg; }
	case 95:  { static const char msg[] PROGMEM = "5F "; return msg; }
	case 96:  { static const char msg[] PROGMEM = "60 "; return msg; }
	case 97:  { static const char msg[] PROGMEM = "61 "; return msg; }
	case 98:  { static const char msg[] PROGMEM = "62 "; return msg; }
	case 99:  { static const char msg[] PROGMEM = "63 "; return msg; }
	case 100: { static const char msg[] PROGMEM = "64 "; return msg; }
	case 101: { static const char msg[] PROGMEM = "65 "; return msg; }
	case 102: { static const char msg[] PROGMEM = "66 "; return msg; }
	case 103: { static const char msg[] PROGMEM = "67 "; return msg; }
	case 104: { static const char msg[] PROGMEM = "68 "; return msg; }
	case 105: { static const char msg[] PROGMEM = "69 "; return msg; }
	case 106: { static const char msg[] PROGMEM = "6A "; return msg; }
	case 107: { static const char msg[] PROGMEM = "6B "; return msg; }
	case 108: { static const char msg[] PROGMEM = "6C "; return msg; }
	case 109: { static const char msg[] PROGMEM = "6D "; return msg; }
	case 110: { static const char msg[] PROGMEM = "6E "; return msg; }
	case 111: { static const char msg[] PROGMEM = "6F "; return msg; }
	case 112: { static const char msg[] PROGMEM = "70 "; return msg; }
	case 113: { static const char msg[] PROGMEM = "71 "; return msg; }
	case 114: { static const char msg[] PROGMEM = "72 "; return msg; }
	case 115: { static const char msg[] PROGMEM = "73 "; return msg; }
	case 116: { static const char msg[] PROGMEM = "74 "; return msg; }
	case 117: { static const char msg[] PROGMEM = "75 "; return msg; }
	case 118: { static const char msg[] PROGMEM = "76 "; return msg; }
	case 119: { static const char msg[] PROGMEM = "77 "; return msg; }
	case 120: { static const char msg[] PROGMEM = "78 "; return msg; }
	case 121: { static const char msg[] PROGMEM = "79 "; return msg; }
	case 122: { static const char msg[] PROGMEM = "7A "; return msg; }
	case 123: { static const char msg[] PROGMEM = "7B "; return msg; }
	case 124: { static const char msg[] PROGMEM = "7C "; return msg; }
	case 125: { static const char msg[] PROGMEM = "7D "; return msg; }
	case 126: { static const char msg[] PROGMEM = "7E "; return msg; }
	case 127: { static const char msg[] PROGMEM = "7F "; return msg; }
	case 128: { static const char msg[] PROGMEM = "80 "; return msg; }
	case 129: { static const char msg[] PROGMEM = "81 "; return msg; }
	case 130: { static const char msg[] PROGMEM = "82 "; return msg; }
	case 131: { static const char msg[] PROGMEM = "83 "; return msg; }
	case 132: { static const char msg[] PROGMEM = "84 "; return msg; }
	case 133: { static const char msg[] PROGMEM = "85 "; return msg; }
	case 134: { static const char msg[] PROGMEM = "86 "; return msg; }
	case 135: { static const char msg[] PROGMEM = "87 "; return msg; }
	case 136: { static const char msg[] PROGMEM = "88 "; return msg; }
	case 137: { static const char msg[] PROGMEM = "89 "; return msg; }
	case 138: { static const char msg[] PROGMEM = "8A "; return msg; }
	case 139: { static const char msg[] PROGMEM = "8B "; return msg; }
	case 140: { static const char msg[] PROGMEM = "8C "; return msg; }
	case 141: { static const char msg[] PROGMEM = "8D "; return msg; }
	case 142: { static const char msg[] PROGMEM = "8E "; return msg; }
	case 143: { static const char msg[] PROGMEM = "8F "; return msg; }
	case 144: { static const char msg[] PROGMEM = "90 "; return msg; }
	case 145: { static const char msg[] PROGMEM = "91 "; return msg; }
	case 146: { static const char msg[] PROGMEM = "92 "; return msg; }
	case 147: { static const char msg[] PROGMEM = "93 "; return msg; }
	case 148: { static const char msg[] PROGMEM = "94 "; return msg; }
	case 149: { static const char msg[] PROGMEM = "95 "; return msg; }
	case 150: { static const char msg[] PROGMEM = "96 "; return msg; }
	case 151: { static const char msg[] PROGMEM = "97 "; return msg; }
	case 152: { static const char msg[] PROGMEM = "98 "; return msg; }
	case 153: { static const char msg[] PROGMEM = "99 "; return msg; }
	case 154: { static const char msg[] PROGMEM = "9A "; return msg; }
	case 155: { static const char msg[] PROGMEM = "9B "; return msg; }
	case 156: { static const char msg[] PROGMEM = "9C "; return msg; }
	case 157: { static const char msg[] PROGMEM = "9D "; return msg; }
	case 158: { static const char msg[] PROGMEM = "9E "; return msg; }
	case 159: { static const char msg[] PROGMEM = "9F "; return msg; }
	case 160: { static const char msg[] PROGMEM = "A0 "; return msg; }
	case 161: { static const char msg[] PROGMEM = "A1 "; return msg; }
	case 162: { static const char msg[] PROGMEM = "A2 "; return msg; }
	case 163: { static const char msg[] PROGMEM = "A3 "; return msg; }
	case 164: { static const char msg[] PROGMEM = "A4 "; return msg; }
	case 165: { static const char msg[] PROGMEM = "A5 "; return msg; }
	case 166: { static const char msg[] PROGMEM = "A6 "; return msg; }
	case 167: { static const char msg[] PROGMEM = "A7 "; return msg; }
	case 168: { static const char msg[] PROGMEM = "A8 "; return msg; }
	case 169: { static const char msg[] PROGMEM = "A9 "; return msg; }
	case 170: { static const char msg[] PROGMEM = "AA "; return msg; }
	case 171: { static const char msg[] PROGMEM = "AB "; return msg; }
	case 172: { static const char msg[] PROGMEM = "AC "; return msg; }
	case 173: { static const char msg[] PROGMEM = "AD "; return msg; }
	case 174: { static const char msg[] PROGMEM = "AE "; return msg; }
	case 175: { static const char msg[] PROGMEM = "AF "; return msg; }
	case 176: { static const char msg[] PROGMEM = "B0 "; return msg; }
	case 177: { static const char msg[] PROGMEM = "B1 "; return msg; }
	case 178: { static const char msg[] PROGMEM = "B2 "; return msg; }
	case 179: { static const char msg[] PROGMEM = "B3 "; return msg; }
	case 180: { static const char msg[] PROGMEM = "B4 "; return msg; }
	case 181: { static const char msg[] PROGMEM = "B5 "; return msg; }
	case 182: { static const char msg[] PROGMEM = "B6 "; return msg; }
	case 183: { static const char msg[] PROGMEM = "B7 "; return msg; }
	case 184: { static const char msg[] PROGMEM = "B8 "; return msg; }
	case 185: { static const char msg[] PROGMEM = "B9 "; return msg; }
	case 186: { static const char msg[] PROGMEM = "BA "; return msg; }
	case 187: { static const char msg[] PROGMEM = "BB "; return msg; }
	case 188: { static const char msg[] PROGMEM = "BC "; return msg; }
	case 189: { static const char msg[] PROGMEM = "BD "; return msg; }
	case 190: { static const char msg[] PROGMEM = "BE "; return msg; }
	case 191: { static const char msg[] PROGMEM = "BF "; return msg; }
	case 192: { static const char msg[] PROGMEM = "C0 "; return msg; }
	case 193: { static const char msg[] PROGMEM = "C1 "; return msg; }
	case 194: { static const char msg[] PROGMEM = "C2 "; return msg; }
	case 195: { static const char msg[] PROGMEM = "C3 "; return msg; }
	case 196: { static const char msg[] PROGMEM = "C4 "; return msg; }
	case 197: { static const char msg[] PROGMEM = "C5 "; return msg; }
	case 198: { static const char msg[] PROGMEM = "C6 "; return msg; }
	case 199: { static const char msg[] PROGMEM = "C7 "; return msg; }
	case 200: { static const char msg[] PROGMEM = "C8 "; return msg; }
	case 201: { static const char msg[] PROGMEM = "C9 "; return msg; }
	case 202: { static const char msg[] PROGMEM = "CA "; return msg; }
	case 203: { static const char msg[] PROGMEM = "CB "; return msg; }
	case 204: { static const char msg[] PROGMEM = "CC "; return msg; }
	case 205: { static const char msg[] PROGMEM = "CD "; return msg; }
	case 206: { static const char msg[] PROGMEM = "CE "; return msg; }
	case 207: { static const char msg[] PROGMEM = "CF "; return msg; }
	case 208: { static const char msg[] PROGMEM = "D0 "; return msg; }
	case 209: { static const char msg[] PROGMEM = "D1 "; return msg; }
	case 210: { static const char msg[] PROGMEM = "D2 "; return msg; }
	case 211: { static const char msg[] PROGMEM = "D3 "; return msg; }
	case 212: { static const char msg[] PROGMEM = "D4 "; return msg; }
	case 213: { static const char msg[] PROGMEM = "D5 "; return msg; }
	case 214: { static const char msg[] PROGMEM = "D6 "; return msg; }
	case 215: { static const char msg[] PROGMEM = "D7 "; return msg; }
	case 216: { static const char msg[] PROGMEM = "D8 "; return msg; }
	case 217: { static const char msg[] PROGMEM = "D9 "; return msg; }
	case 218: { static const char msg[] PROGMEM = "DA "; return msg; }
	case 219: { static const char msg[] PROGMEM = "DB "; return msg; }
	case 220: { static const char msg[] PROGMEM = "DC "; return msg; }
	case 221: { static const char msg[] PROGMEM = "DD "; return msg; }
	case 222: { static const char msg[] PROGMEM = "DE "; return msg; }
	case 223: { static const char msg[] PROGMEM = "DF "; return msg; }
	case 224: { static const char msg[] PROGMEM = "E0 "; return msg; }
	case 225: { static const char msg[] PROGMEM = "E1 "; return msg; }
	case 226: { static const char msg[] PROGMEM = "E2 "; return msg; }
	case 227: { static const char msg[] PROGMEM = "E3 "; return msg; }
	case 228: { static const char msg[] PROGMEM = "E4 "; return msg; }
	case 229: { static const char msg[] PROGMEM = "E5 "; return msg; }
	case 230: { static const char msg[] PROGMEM = "E6 "; return msg; }
	case 231: { static const char msg[] PROGMEM = "E7 "; return msg; }
	case 232: { static const char msg[] PROGMEM = "E8 "; return msg; }
	case 233: { static const char msg[] PROGMEM = "E9 "; return msg; }
	case 234: { static const char msg[] PROGMEM = "EA "; return msg; }
	case 235: { static const char msg[] PROGMEM = "EB "; return msg; }
	case 236: { static const char msg[] PROGMEM = "EC "; return msg; }
	case 237: { static const char msg[] PROGMEM = "ED "; return msg; }
	case 238: { static const char msg[] PROGMEM = "EE "; return msg; }
	case 239: { static const char msg[] PROGMEM = "EF "; return msg; }
	case 240: { static const char msg[] PROGMEM = "F0 "; return msg; }
	case 241: { static const char msg[] PROGMEM = "F1 "; return msg; }
	case 242: { static const char msg[] PROGMEM = "F2 "; return msg; }
	case 243: { static const char msg[] PROGMEM = "F3 "; return msg; }
	case 244: { static const char msg[] PROGMEM = "F4 "; return msg; }
	case 245: { static const char msg[] PROGMEM = "F5 "; return msg; }
	case 246: { static const char msg[] PROGMEM = "F6 "; return msg; }
	case 247: { static const char msg[] PROGMEM = "F7 "; return msg; }
	case 248: { static const char msg[] PROGMEM = "F8 "; return msg; }
	case 249: { static const char msg[] PROGMEM = "F9 "; return msg; }
	case 250: { static const char msg[] PROGMEM = "FA "; return msg; }
	case 251: { static const char msg[] PROGMEM = "FB "; return msg; }
	case 252: { static const char msg[] PROGMEM = "FC "; return msg; }
	case 253: { static const char msg[] PROGMEM = "FD "; return msg; }
	case 254: { static const char msg[] PROGMEM = "FE "; return msg; }
	case 255:
	default:
		{ static const char msg[] PROGMEM = "FF "; return msg; }
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

#ifdef USE_BUZZER
	buzzer_update(increment);
#endif
}

#ifdef USE_BUZZER
static uint16_t buzzer_ms;

static const int TIMER_MODE = ((1<<WGM01) | (1<<CS01) | (1<<CS00));

void buzzer_start(uint16_t ms){
	if(buzzer_ms <= ms){
		buzzer_ms = ms;

		// Turn on the buzzer and start the timer
		BUZZER_PORT |= BUZZER;

		TCNT0  = 0;
		OCR0   = 150;
		TCCR0 |= TIMER_MODE;
		TIMSK |= (1<<OCIE0);
	}
}

void buzzer_update(uint8_t increment){
	if(buzzer_ms){
		buzzer_ms = (increment >= buzzer_ms) ? 0 : buzzer_ms - increment;
		if(buzzer_ms == 0){
			// Stop the timer and turn off the buzzer
			TCCR0       &= ~TIMER_MODE;
			BUZZER_PORT &= ~BUZZER;
		}
	}
}

ISR(TIMER0_COMP_vect){
	BUZZER_PORT ^= BUZZER;
}

#endif
