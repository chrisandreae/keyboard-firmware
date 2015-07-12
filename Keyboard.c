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
#include "macro_index.h"
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

	// Low pitched buzz on startup
	buzzer_start_f(200, 200);

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

		USB_Perform_Update();
	}
}

static void handle_state_normal(void){
	if(key_press_count == 0 || key_press_count > MACRO_MAX_KEYS){
		return;
	}

	// Read current logical keys into macro trigger structure
	macro_idx_key macro_key;
	keystate_get_keys(macro_key.keys, LOGICAL);

	// check for special program key combinations
	if(key_press_count >= 2 && key_press_count <= 3){
		hid_keycode hid_keys[3];
		for(uint8_t i = 0; i < key_press_count; ++i){
			hid_keys[i] = config_get_definition(macro_key.keys[i]);
		}

		insertionsort_uint8(hid_keys, key_press_count);

		if(hid_keys[key_press_count - 1] == SPECIAL_HID_KEY_PROGRAM){
			// Potentially a special program key combination
			switch(key_press_count){
			case 2:
				switch(hid_keys[0]){
				case SPECIAL_HKEY_MACRO_RECORD:
					current_state = STATE_WAITING;
					next_state = STATE_MACRO_RECORD_TRIGGER;
					return;
				case SPECIAL_HKEY_REMAP:
					current_state = STATE_WAITING;
					next_state = STATE_PROGRAMMING_SRC;
					return;
				case SPECIAL_HKEY_REBOOT: {
					uint8_t i = BUZZER_DEFAULT_TONE;
					// cause watchdog reboot (into bootloader if progm is still pressed)
					while(1){
#if USE_BUZZER
						// Beep until rebooted
						buzzer_start_f(100, i);
#endif
						i -= 10;
						_delay_ms(100);
						Update_Millis(100);
						_delay_ms(100);
					}
				}
#if USE_BUZZER
				case SPECIAL_HKEY_TOGGLE_BUZZER: {
					configuration_flags flags = config_get_flags();
					flags.key_sound_enabled = !flags.key_sound_enabled;
					config_save_flags(flags);
					buzzer_start_f(100, flags.key_sound_enabled ? BUZZER_ON_TONE : BUZZER_OFF_TONE);

					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
					return;
				}
#endif
				case SPECIAL_HKEY_RESET_CONFIG:
					config_reset_defaults();
					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
					return;
				default:
					break;
				}
				break;
			case 3:
				if(hid_keys[SPECIAL_HKEY_RESET_CONFIG_POS] == SPECIAL_HKEY_RESET_CONFIG &&
                   hid_keys[!SPECIAL_HKEY_RESET_CONFIG_POS] == SPECIAL_HKEY_RESET_FULLY){
					// full reset
					config_reset_fully();
					current_state = STATE_WAITING;
					next_state = STATE_NORMAL;
					return;
				}
				else if(hid_keys[1] >= HID_KEYBOARD_SC_1_AND_EXCLAMATION && hid_keys[1] <= HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS){
					// operation on saved layout n
					uint8_t index = hid_keys[1] - HID_KEYBOARD_SC_1_AND_EXCLAMATION;

					bool success;
					switch(hid_keys[0]){
					case HID_KEYBOARD_SC_S:
						success = config_save_layout(index);
						break;
					case HID_KEYBOARD_SC_L:
						success = config_load_layout(index);
						break;
					case HID_KEYBOARD_SC_D:
						success = config_delete_layout(index);
						break;
					default:
						goto no_combo;
					}
				
					if(success){
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
					return;
				no_combo:;
				}
				break;
			default:
				break;
			}
		}
	}

	// otherwise, check macro/program triggers
	bool valid = macro_idx_format_key(&macro_key, key_press_count);
	if(!valid) return;

	macro_idx_entry* h = macro_idx_lookup(&macro_key);
	if(h){
		macro_idx_entry_data md = macro_idx_get_data(h);
		switch(md.type){
		case PROGRAM: {
#if PROGRAMS_SIZE > 0
			vm_start(md.data, macro_key.keys[0]); // TODO: l_key is no longer relevant, is not great to use just the first.
			break;
#endif
		}
		case MACRO: {
#if MACROS_SIZE > 0
			if(macros_start_playback(md.data)){
				current_state = STATE_MACRO_PLAY;
			}
			else{
				buzzer_start_f(200, BUZZER_FAILURE_TONE);
			}
			break;
#endif
		}
		}
	}
}



static void handle_state_programming(void){
	static hid_keycode program_src_hkey = 0;

	if(key_press_count == 0 || key_press_count > 2) return;

	logical_keycode lkeys[2];
	hid_keycode hkeys[2];

	keystate_get_keys(lkeys, LOGICAL);
	for(int i = 0; i < key_press_count; ++i){
		hkeys[i] = config_get_definition(lkeys[i]);
	}

	if(key_press_count == 2){
		// We rely here on the SPECIAL_HID codes for program and keypad being larger
		// than any real HID key.
		uint8_t lesser_idx = (hkeys[0] < hkeys[1]) ? 0 : 1;

		// check for quit
		if(hkeys[lesser_idx] == SPECIAL_HKEY_REMAP && hkeys[!lesser_idx] == SPECIAL_HID_KEY_PROGRAM){
			current_state = STATE_WAITING;
			next_state = STATE_NORMAL;
			return;
		}

		// Check for keypad shift. If not pressed, it's an invalid 2-key
		// combination: return. Otherwise, we have a single keycode in the
		// keypad layer: place it in index 0 and continue.
		if(hkeys[!lesser_idx] != SPECIAL_HID_KEY_KEYPAD_SHIFT){
			return;
		}
		if(lesser_idx != 0){
			lkeys[0] = lkeys[lesser_idx];
			hkeys[0] = hkeys[lesser_idx];
		}
	}

	// can't interactively reprogram a special "noremap" key type such as program or keypad.
	if(SPECIAL_HID_KEY_NOREMAP(hkeys[0])){
		return;
	}

	// Otherwise we're ready to remap the logical position in lkeys[0]
	logical_keycode lkey = lkeys[0];

	if(current_state == STATE_PROGRAMMING_SRC){
		program_src_hkey = config_get_default_definition(lkey);
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
#if MACROS_SIZE > 0 // Allow macro recording only if there's storage for it.
	static macro_idx_key key;
	static uint8_t last_count = 0;
	if(keystate_check_keys(2, HID, SPECIAL_HID_KEY_PROGRAM, SPECIAL_HKEY_MACRO_RECORD)){
		current_state = STATE_WAITING;
		next_state = STATE_NORMAL;
		return;
	}
	else if(keystate_check_any_key(2, HID, SPECIAL_HID_KEY_PROGRAM, SPECIAL_HID_KEY_KEYPAD_TOGGLE)){
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
		// Last recorded `key` is our trigger.
		bool valid = macro_idx_format_key(&key, last_count);
		last_count = 0;

		if(!valid){
			return; // only keypad shift was pressed: keep trying to record a trigger.
		}
		else if(macros_start_macro(&key)){
			current_state = STATE_WAITING;
			next_state = STATE_MACRO_RECORD;
		}
		else{
#else
		{{
#endif
			// failed to start macro
			current_state = STATE_WAITING;
			next_state = STATE_NORMAL;
		}
	}
}

static bool recording_macro = false;

static void macro_record_hook(logical_keycode key, bool press){
	if(keystate_check_key(SPECIAL_HID_KEY_PROGRAM, HID)){
		return; // ignore all events if program is pressed
	}
	hid_keycode h_key = config_get_definition(key);
	if(h_key >= SPECIAL_HID_KEYS_START){
		return; // Currently don't allow special keys to participate in macros
	}
	bool success = macros_append(h_key);
	if(!success){
		recording_macro = false;
		keystate_register_change_hook(NULL);
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
	if(keystate_check_keys(2, HID, SPECIAL_HID_KEY_PROGRAM, SPECIAL_HKEY_MACRO_RECORD)){
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
		if(!macros_fill_next_report(KeyboardReport)){
			current_state = STATE_WAITING;
			next_state = STATE_NORMAL;
		}
		return;
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

	if(keystate_is_keypad_mode()){
		LEDMask |= LEDMASK_KEYPAD;
	}

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
