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

#include "usb.h"
#include "hardware.h"
#include "keystate.h"
#include "config.h"
#include "printing.h"
#include "buzzer.h"
#include "leds.h"

#include "serial_eeprom.h"
#include "interpreter.h"
#include "macro.h"

#include "sort.h"

#include <avr/interrupt.h>
#include <util/delay.h>
#include <stdarg.h>
#include <stdlib.h>

/** Buffer to hold the previously generated Keyboard HID report, for comparison purposes inside the HID class driver. */
KeyboardReport_Data_t PrevKeyboardHIDReportBuffer;

/** Buffer to hold the previously generated Mouse HID report, for comparison purposes inside the HID class driver. */
MouseReport_Data_t PrevMouseHIDReportBuffer;

// Keyboard
volatile uint32_t _uptimems;

// Messages
static state current_state = STATE_NORMAL;
// state to transition to when next action is complete:
// used for STATE_WAITING, STATE_PRINTING and STATE_EEWRITE which might transition into multiple states
static state next_state;

// Macro playback state
static macro_playback macro_playback_state;

// Predeclarations
static void handle_state_normal(void);
static void handle_state_programming(void);
static void handle_state_macro_record_trigger(void);
static void handle_state_macro_record(void);
static void ledstate_update(void);

static void print_pgm_message(const char* buffer, state next){
	printing_set_buffer(buffer, BUF_PGM);
	current_state = STATE_PRINTING;
	next_state = next;
}

/** Main program entry point. This routine contains the overall program flow, including initial
 *  setup of all components and the main program loop.
 */
void __attribute__((noreturn)) Keyboard_Main(void)
{
	ports_init();
	keystate_init();
	config_init();
	vm_init();

	sei();

#if USE_BUZZER
	// Low pitched buzz on startup
	buzzer_start_f(200, 200);
#endif

	struct { int keys:1; int mouse:1; } update;

	for (;;) {
		// update key state once per 2ms slice
		uint8_t slice = (uptimems() & 0x1);
		if(!slice && update.keys){
			keystate_update();
			ledstate_update();
			update.keys = 0;
		}
		else if(!update.keys && slice){
			update.keys = 1;
		}

		// in all non-wait states we want to handle the keypad layer button
#ifdef KEYPAD_LAYER
		if(current_state != STATE_WAITING && keystate_check_key(LOGICAL_KEY_KEYPAD, LOGICAL)){
			keystate_toggle_keypad();
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
			if(printing_buffer_empty()){
				current_state = STATE_WAITING;
				/* next_state = 0; */
			}
			break;
		case STATE_PROGRAMMING_SRC:
		case STATE_PROGRAMMING_DST:
			handle_state_programming();
			break;
		case STATE_MACRO_RECORD_TRIGGER:
			handle_state_macro_record_trigger();
			break;
		case STATE_MACRO_RECORD:
			handle_state_macro_record();
			break;
		case STATE_MACRO_PLAY:
			// macro playback is handled entirely by macros_fill_next_report()
			break;
		default: {
			print_pgm_message(PGM_MSG("Unexpected state"), STATE_NORMAL);
			break;
		}
		}

		if(current_state == STATE_NORMAL){
			vm_step_all();
		}

		/* Limit frequency of mouse reports. Unlike keyboard reports,
		   identical reports won't be ignored by the class driver, so
		   report speed affects mouse movement speed. */
		uint8_t mouse_slice = (uptimems() & 0x8);
		uint8_t perform_mouse_update = 0;
		if(!mouse_slice && update.mouse){
			perform_mouse_update = 1;
			update.mouse = 0;
		}
		else if(!update.mouse && mouse_slice){
			update.mouse = 1;
		}

		USB_Perform_Update(1, perform_mouse_update);
	}
}

static void handle_state_normal(void){
	// check for special keyboard (pre-mapping) key combinations for state transitions

	if(key_press_count >= 2 && keystate_check_key(LOGICAL_KEY_PROGRAM, LOGICAL)){
		switch(key_press_count){
		case 2:
			{
				logical_keycode keys[2];
				keystate_get_keys(keys, PHYSICAL);
				logical_keycode other = (keys[0] == LOGICAL_KEY_PROGRAM) ? keys[1] : keys[0];
				switch(other){
				case LOGICAL_KEY_F11:
					current_state = STATE_WAITING;
					next_state = STATE_MACRO_RECORD_TRIGGER;
					break;
				case LOGICAL_KEY_F12:
					current_state = STATE_WAITING;
					next_state = STATE_PROGRAMMING_SRC;
					break;
				case LOGICAL_KEY_PRINTSCREEN: {
					uint8_t i = BUZZER_DEFAULT_TONE;
						// cause watchdog reboot (into bootloader if progm is still pressed)
					while(1){
						// Beep until rebooted
						buzzer_start_f(100, i);
						i -= 10;
						_delay_ms(100);
						Update_Millis(100);
						_delay_ms(100);
					}
				}
#if USE_BUZZER
				case LOGICAL_KEY_BACKSLASH: {
					configuration_flags flags = config_get_flags();
					flags.key_sound_enabled = !flags.key_sound_enabled;
					config_save_flags(flags);
					buzzer_start_f(100, flags.key_sound_enabled ? BUZZER_ON_TONE : BUZZER_OFF_TONE);

					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
					break;
				}
#endif
				case LOGICAL_KEY_F7:
					config_reset_defaults();
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
			if(keystate_check_keys(2, PHYSICAL, LOGICAL_KEY_F7, LOGICAL_KEY_LSHIFT)){
				config_reset_fully();
				current_state = STATE_WAITING;
				next_state = STATE_NORMAL;
			}
			else{
				logical_keycode keys[3];
				keystate_get_keys(keys, PHYSICAL);

				// save/load/delete state : PGM + {S/L/D} + {0-9}
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
					r = config_save_layout(index);
				} else if(type == LOGICAL_KEY_L) {
					r = config_load_layout(index);
				} else {
					r = config_delete_layout(index);
				}
				if(r){
					buzzer_start_f(200, BUZZER_SUCCESS_TONE); // high buzz for success
					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
				}
				else{
					// failure - we have put an error msg in print_buffer
					buzzer_start_f(200, BUZZER_FAILURE_TONE); // low buzz for error
					current_state = STATE_PRINTING;
					next_state = STATE_NORMAL;
				}
			}
			break;
		default:
			break;
		}

	}

	// otherwise, check macro triggers
	if(key_press_count && key_press_count <= MACRO_MAX_KEYS){
		// Read keys
		macro_key key;
		keystate_get_keys(key.keys, LOGICAL);
		insertionsort_uint8(key.keys, key_press_count);
		for(uint8_t i = key_press_count; i < MACRO_MAX_KEYS; ++i){
			key.keys[i] = NO_KEY;
		}
		macro_data* m = macros_lookup(&key);
		if(m != NO_MACRO){
			ExtraKeyboardReport_clear(&macro_playback_state.report);
			macro_playback_state.cursor = &m->events[0];
			if(serial_eeprom_read((uint8_t*)&m->length, (uint8_t*)&macro_playback_state.remaining, sizeof(macro_playback_state.remaining))){
				current_state = STATE_MACRO_PLAY;
			}
			else{
				// failure to read macro data
				buzzer_start_f(200, BUZZER_FAILURE_TONE);
			}
		}
	}

	// If we are still in state normal, handle programs
	if(current_state == STATE_NORMAL){
		keystate_run_programs();
	}
}

static void handle_state_programming(void){
	static hid_keycode program_src_hkey = 0;

	if(keystate_check_keys(2, PHYSICAL, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F12)){
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
	}

	if(key_press_count != 1){
		return;
	}

	logical_keycode lkey;
	keystate_get_keys(&lkey, LOGICAL); // Will only write one key, as key_press_count == 1

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
		config_save_definition(lkey, program_src_hkey);
		current_state = STATE_WAITING;
		next_state = STATE_PROGRAMMING_SRC;
	}
}

static void handle_state_macro_record_trigger(){
	static macro_key key;
	static uint8_t last_count = 0;
	if(keystate_check_keys(2, PHYSICAL, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F11)){
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
		return;
	}
	else if(keystate_check_key(LOGICAL_KEY_PROGRAM, PHYSICAL) || keystate_check_key(LOGICAL_KEY_KEYPAD, PHYSICAL)){
		return; // ignore
	}
	else if(key_press_count > MACRO_MAX_KEYS){
		// too many, give up
		buzzer_start_f(200, BUZZER_FAILURE_TONE);
		last_count = 0;
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
		return;
	}
	else if(key_press_count >= last_count){
		keystate_get_keys(key.keys, LOGICAL);
		last_count = key_press_count;
	}
	else{
		// last is our trigger. Sort and clear remaining keys
		insertionsort_uint8(key.keys, last_count);
		for(uint8_t i = last_count; i < MACRO_MAX_KEYS; ++i){
			key.keys[i] = NO_KEY;
		}
		last_count = 0;
		if(macros_start_macro(&key)){
			current_state = STATE_WAITING;
			next_state = STATE_MACRO_RECORD;
		}
		else{
			// failed to start macro
			current_state = STATE_WAITING;
			next_state = STATE_NORMAL;
		}
	}
}

static bool recording_macro = false;

static void macro_record_hook(logical_keycode key, bool press){
	if(keystate_check_key(LOGICAL_KEY_PROGRAM, PHYSICAL) || keystate_check_key(LOGICAL_KEY_KEYPAD, PHYSICAL)){
		return; // ignore all events when program or keypad are pressed
	}
	hid_keycode h_key = config_get_definition(key);
	if(h_key >= SPECIAL_HID_KEYS_START){
		return; // Currently don't allow special keys to participate in macros
	}
	bool success = macros_append(h_key);
	if(!success){
		recording_macro = false;
		keystate_register_change_hook(0);
		buzzer_start_f(200, BUZZER_FAILURE_TONE);
		macros_abort_macro();
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
	}
}

static void handle_state_macro_record(){
	if(!recording_macro){
		recording_macro = true;
		keystate_register_change_hook(macro_record_hook);
	}

	// handle stopping
	if(keystate_check_keys(2, PHYSICAL, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F11)){
		recording_macro = false;
		keystate_register_change_hook(0);
		macros_commit_macro();
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
	}
}

/**
 * Fills the argument buffer with a keyboard report according to the
 * current state returns true if the report must be sent, false if it
 * may be compared to the previous report before sending.
 */
void Fill_KeyboardReport(KeyboardReport_Data_t* KeyboardReport){
	switch(current_state){
	case STATE_NORMAL:
		keystate_Fill_KeyboardReport(KeyboardReport);
		vm_append_KeyboardReport(KeyboardReport);
		return;
	case STATE_PRINTING:
		printing_Fill_KeyboardReport(KeyboardReport);
		return;
	case STATE_MACRO_RECORD:
		// When recording a macro we want to pass through the events.
		// They will also be recorded via the keystate change hook.
		keystate_Fill_KeyboardReport(KeyboardReport);
		return;
	case STATE_MACRO_PLAY:
		if(!macros_fill_next_report(&macro_playback_state, KeyboardReport)){
			current_state = STATE_WAITING;
			next_state = STATE_NORMAL;
		}
		return;
		// TODO: Fetch the next report from the macro buffer and replay it
	case STATE_PROGRAMMING_SRC:
	case STATE_PROGRAMMING_DST:
	default:
		// We're not in a state which allows typing: report no keys
		return;
	}
}

void Fill_MouseReport(MouseReport_Data_t* MouseReport){
	switch(current_state){
	case STATE_NORMAL:{
		keystate_Fill_MouseReport(MouseReport);
		vm_append_MouseReport(MouseReport);
		return;
	}
	case STATE_MACRO_RECORD: {
		keystate_Fill_MouseReport(MouseReport);
		// TODO: If this report is different to the previous one, save it in the macro buffer.
		return;
	}
	case STATE_MACRO_PLAY:
		// TODO: Fetch the next report from the macro buffer and replay it
	case STATE_PRINTING:
	case STATE_PROGRAMMING_SRC:
	case STATE_PROGRAMMING_DST:
	default:
		// We're not in a state which allows typing: report no keys
		return;
	}

}

static uint8_t USB_LEDReport = 0;

void Process_KeyboardLEDReport(uint8_t report){
	USB_LEDReport = report;
}

static void ledstate_update(void){
	uint8_t LEDMask = 0;

#ifdef KEYPAD_LAYER
		if(keypad_mode)
			LEDMask |= LEDMASK_KEYPAD;
#endif

	switch(current_state){
	case STATE_PROGRAMMING_SRC:
		// flash quickly - change every 128ms
		if(uptimems() & 128){
			LEDMask |= LEDMASK_PROGRAMMING_SRC;
		}
		break;
	case STATE_PROGRAMMING_DST:
		// flash slowly - change every 256ms
		if(uptimems() & 256){
			LEDMask |= LEDMASK_PROGRAMMING_DST;
		}
		break;
	case STATE_MACRO_RECORD_TRIGGER:
		if(uptimems() & 128){
			LEDMask |= LEDMASK_MACRO_TRIGGER;
		}
		break;
	case STATE_MACRO_RECORD:
		if(uptimems() & 128){
			LEDMask |= LEDMASK_MACRO_RECORD;
		}
		break;
	case STATE_NORMAL:
	default:
		// populate from USB LED report
		if (USB_LEDReport & HID_KEYBOARD_LED_NUMLOCK)
			LEDMask |= LEDMASK_NUMLOCK;

		if (USB_LEDReport & HID_KEYBOARD_LED_CAPSLOCK)
			LEDMask |= LEDMASK_CAPS;

#ifdef LEDMASK_SCROLLLOCK
		if (USB_LEDReport & HID_KEYBOARD_LED_SCROLLLOCK)
			LEDMask |= LEDMASK_SCROLLLOCK;
#endif

		break;
	}

	set_all_leds(LEDMask);
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
	_uptimems += increment;

#if USE_BUZZER
	buzzer_update(increment);
#endif

}
