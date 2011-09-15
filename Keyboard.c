/*
			 LUFA Library
	 Copyright (C) Dean Camera, 2011.

  dean [at] fourwalledcubicle [dot] com
		   www.lufa-lib.org
*/

/*
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

/** \file
 *
 *  Main source file for the Keyboard demo. This file contains the main tasks of
 *  the demo and is responsible for the initial application hardware configuration.
 */

#include "Keyboard.h"
#include "avr/eeprom.h"
#include <stdarg.h>

/** Buffer to hold the previously generated Keyboard HID report, for comparison purposes inside the HID class driver. */
static uint8_t PrevKeyboardHIDReportBuffer[sizeof(USB_KeyboardReport_Data_t)];

// time in ms since boot, wraps every 32 days or so.
static uint32_t uptimems = 0;

#define NO_KEY 0xFF

/* Logical keys correspond to HID codes. We put some special keys after E7, the last HID key */
/* These keys cannot be remapped, and the codes will never be sent by USB */
#define SPECIAL_HID_KEY_PROGRAM 0xE8
#define SPECIAL_HID_KEY_KEYPAD 0xE9
#define SPECIAL_HID_KEY_EXECUTE_PROGRAM 0xF0 // run the program associated with the index position
#define SPECIAL_HID_KEYS_START SPECIAL_HID_KEY_PROGRAM


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
#define EEPROM_SENTINEL 41
uint8_t eeprom_sentinel_byte EEMEM;

// Key configuration is stored in eeprom. If the sentinel is not valid, initialize from the defaults.
hid_keycode logical_to_hid_map[NUM_LOGICAL_KEYS] EEMEM;

// State of active keys. Keep track of all pressed or debouncing keys.
#define KEYSTATE_COUNT 14
static key_state key_states[KEYSTATE_COUNT];
static uint8_t key_press_count = 0;

/** LUFA HID Class driver interface configuration and state information. This structure is
 *  passed to all HID Class driver functions, so that multiple instances of the same class
 *  within a device can be differentiated from one another.
 */
USB_ClassInfo_HID_Device_t Keyboard_HID_Interface =
 	{
		.Config =
			{
				.InterfaceNumber              = 0,

				.ReportINEndpointNumber       = KEYBOARD_EPNUM,
				.ReportINEndpointSize         = KEYBOARD_EPSIZE,
				.ReportINEndpointDoubleBank   = false,

				.PrevReportINBuffer           = PrevKeyboardHIDReportBuffer,
				.PrevReportINBufferSize       = sizeof(PrevKeyboardHIDReportBuffer),
			},
	};

// Buffer for printing.
static char* print_buffer;

static state current_state = STATE_NORMAL;

// state to transition to when next action is complete:
// used for STATE_WAITING, STATE_PRINTING and STATE_EEWRITE which might transition into multiple states
static state next_state;

#ifdef KEYPAD_LAYER
static uint8_t keypad_mode;
#endif

/** Main program entry point. This routine contains the overall program flow, including initial
 *  setup of all components and the main program loop.
 */
int main(void)
{
	SetupHardware();

	set_all_leds(LEDMASK_USB_NOTREADY);
	sei();

	uint8_t update = 1;

	hid_keycode program_src_hkey = 0;
	for (;;) {
		// update key state once per 2ms slice
		uint8_t slice = (uptimems & 0x1);
		if(!slice && update){
			KeyState_Update();
			updateLEDs();
			update = 0;
		}
		else if(!update && slice){
			update = 1;
		}

		switch(current_state){
		case STATE_NORMAL:
			// check for special keyboard (pre-mapping) key combinations for state transitions

			if(KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F11)){
				print_buffer = "no macro support yet";
				current_state = STATE_PRINTING;
				next_state = STATE_NORMAL;
			}
			else if(KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F12)){
				current_state = STATE_WAITING;
				next_state = STATE_PROGRAMMING_SRC;
			}
			else if(KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F7)){
				Eeprom_ResetDefaults();
				current_state = STATE_WAITING;
				next_state = STATE_NORMAL;
			}
#ifdef KEYPAD_LAYER
			else if(KeyState_CheckKey(LOGICAL_KEY_KEYPAD)){
				KeyState_ToggleKeypad();
				current_state = STATE_WAITING;
				next_state = STATE_NORMAL;
			}
#endif
			break;
		case STATE_WAITING:
			if(key_press_count == 0){
				current_state = next_state;
				next_state = 0;
			}
			break;
		case STATE_PRINTING:
			if(*print_buffer == '\0'){
				current_state = STATE_WAITING;
				/* next_state = 0; */
			}
			break;
		case STATE_PROGRAMMING_SRC:
			// if there is a key press, record it as the programming source key
			// then transition to programming b via print
			// consider flashing LEDs, a la Kinesis.
			if(KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F12)){
				current_state = STATE_WAITING;
				next_state = STATE_NORMAL;
			}
#ifdef KEYPAD_LAYER
			else if(KeyState_CheckKey(LOGICAL_KEY_KEYPAD)){
				KeyState_ToggleKeypad();
				current_state = STATE_WAITING;
				next_state = STATE_PROGRAMMING_SRC;
			}
#endif
			else if(key_press_count == 1){
				logical_keycode src_lkey;
				KeyState_GetKeys(&src_lkey); // Will only write one key, as key_press_count == 1

				program_src_hkey = pgm_read_byte_near(&logical_to_hid_map_default[src_lkey]);
				
				// can't reprogram a "special" key type (i.e program, keypad), but NO_KEY is ok.
				if(program_src_hkey >= SPECIAL_HID_KEYS_START && program_src_hkey != NO_KEY){
					break;
				}

				next_state = STATE_PROGRAMMING_DST;
				current_state = STATE_WAITING;
			}

			break;
		case STATE_PROGRAMMING_DST:
			// if key press, do it then print the result
			if(KeyState_CheckKeys(2, LOGICAL_KEY_PROGRAM, LOGICAL_KEY_F12)){
				current_state = STATE_WAITING;
				next_state = STATE_NORMAL;
			}
#ifdef KEYPAD_LAYER
			else if(KeyState_CheckKey(LOGICAL_KEY_KEYPAD)){
				KeyState_ToggleKeypad();
				current_state = STATE_WAITING;
				next_state = STATE_PROGRAMMING_DST;
			}
#endif
			else if(key_press_count == 1){
				logical_keycode dst_lkey;
				KeyState_GetKeys(&dst_lkey); // Will only write one key, as key_press_count == 1

				// can't reprogram a "special" key type (i.e program, keypad), except for NO_KEY
				// (i.e disable a key)
				hid_keycode dst_key_default = pgm_read_byte_near(&logical_to_hid_map_default[dst_lkey]);
				if(dst_key_default >= SPECIAL_HID_KEYS_START && dst_key_default != NO_KEY){
					break;
				}
				
				// ok, copy the default for the key index program_src_idx to index key_idx.
				eeprom_update_byte(&logical_to_hid_map[dst_lkey], program_src_hkey);

				current_state = STATE_WAITING;
				next_state = STATE_PROGRAMMING_SRC;
			}
			break;
		case STATE_MACRO_RECORD:
		case STATE_MACRO_PLAY:
			break;
		case STATE_EEWRITE:
			if(eeprom_is_ready())
				current_state = next_state;
			break;
		}

		HID_Device_USBTask(&Keyboard_HID_Interface);
		USB_USBTask();
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

// returns true if all argument keys are down, and no others
bool KeyState_CheckKeys(uint8_t count, ...){
	if(count != key_press_count) return false;

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

void Eeprom_ResetDefaults(void){
	for(int i = 0; i < NUM_LOGICAL_KEYS; ++i){
		hid_keycode default_key = pgm_read_byte_near(&logical_to_hid_map_default[i]);
		eeprom_update_byte(&logical_to_hid_map[i], default_key);
	}
	eeprom_update_byte(&eeprom_sentinel_byte, EEPROM_SENTINEL);
	// flash LEDs to show that we had to reset
	for(int i = 0; i < 10; ++i){
		set_all_leds(LEDMASK_SCROLLLOCK | LEDMASK_CAPS);
		_delay_ms(50);
		set_all_leds(LEDMASK_SCROLLLOCK | LEDMASK_NUMLOCK);
		_delay_ms(50);
	}
}


void Eeprom_Init(void){
	uint8_t sentinel = eeprom_read_byte(&eeprom_sentinel_byte);
	if(sentinel != EEPROM_SENTINEL){
		Eeprom_ResetDefaults();
	}
}

/** Configures the board hardware and chip peripherals for the demo's functionality. */
void SetupHardware(void)
{
	/* Disable watchdog if enabled by bootloader/fuses */
	MCUSR &= ~(1 << WDRF);
	wdt_disable();

	/* Disable clock division */
	clock_prescale_set(clock_div_1);

	/* Hardware Initialization */
	ports_init();

	KeyState_Init();

	Eeprom_Init();

	USB_Init();

}


/** Event handler for the library USB Connection event. */
void EVENT_USB_Device_Connect(void)
{
	set_all_leds(LEDMASK_USB_ENUMERATING);
}

/** Event handler for the library USB Disconnection event. */
void EVENT_USB_Device_Disconnect(void)
{
	set_all_leds(LEDMASK_USB_NOTREADY);
}

/** Event handler for the library USB Configuration Changed event. */
void EVENT_USB_Device_ConfigurationChanged(void)
{
	bool ConfigSuccess = true;

	ConfigSuccess &= HID_Device_ConfigureEndpoints(&Keyboard_HID_Interface);

	// enable the start-of-frame event (millisecond callback)
	USB_Device_EnableSOFEvents();

	set_all_leds(ConfigSuccess ? LEDMASK_USB_READY : LEDMASK_USB_ERROR);
}

/** Event handler for the library USB Control Request reception event. */
void EVENT_USB_Device_ControlRequest(void)
{
	HID_Device_ProcessControlRequest(&Keyboard_HID_Interface);
}

/** Event handler for the USB device Start Of Frame event. */
void EVENT_USB_Device_StartOfFrame(void)
{
	++uptimems;
	HID_Device_MillisecondElapsed(&Keyboard_HID_Interface);
}

static void Fill_HIDReport_normal(USB_KeyboardReport_Data_t* KeyboardReport){
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

void Fill_HIDReport_printing(USB_KeyboardReport_Data_t* ReportData){
	// if the last report was a key, send empty. Otherwise send the
	// next character from print_buffer
	USB_KeyboardReport_Data_t* prev = (USB_KeyboardReport_Data_t*) PrevKeyboardHIDReportBuffer;
	if(prev->Modifier || prev->KeyCode[0])
		return; // empty report
	else{
		char nextchar = *print_buffer++;
		uint8_t key, mod;
		char_to_keys(nextchar, &key, &mod);
		ReportData->Modifier = mod;
		ReportData->KeyCode[0] = key; //HID_KEYBOARD_SC_A + counter; // key;
		//		counter = (counter + 1) % 26;
	}
}



/** HID class driver callback function for the creation of HID reports to the host.
 *
 *  \param[in]     HIDInterfaceInfo  Pointer to the HID class interface configuration structure being referenced
 *  \param[in,out] ReportID    Report ID requested by the host if non-zero, otherwise callback should set to the generated report ID
 *  \param[in]     ReportType  Type of the report to create, either HID_REPORT_ITEM_In or HID_REPORT_ITEM_Feature
 *  \param[out]    ReportData  Pointer to a buffer where the created report should be stored
 *  \param[out]    ReportSize  Number of bytes written in the report (or zero if no report is to be sent
 *
 *  \return Boolean true to force the sending of the report, false to let the library determine if it needs to be sent
 */
bool CALLBACK_HID_Device_CreateHIDReport(USB_ClassInfo_HID_Device_t* const HIDInterfaceInfo, uint8_t* const ReportID,
										 const uint8_t ReportType, void* ReportData, uint16_t* const ReportSize)
{
	USB_KeyboardReport_Data_t* KeyboardReport = (USB_KeyboardReport_Data_t*)ReportData;

	*ReportSize = sizeof(USB_KeyboardReport_Data_t);

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

/** HID class driver callback function for the processing of HID reports from the host.
 *
 *  \param[in] HIDInterfaceInfo  Pointer to the HID class interface configuration structure being referenced
 *  \param[in] ReportID    Report ID of the received report from the host
 *  \param[in] ReportType  The type of report that the host has sent, either HID_REPORT_ITEM_Out or HID_REPORT_ITEM_Feature
 *  \param[in] ReportData  Pointer to a buffer where the created report has been stored
 *  \param[in] ReportSize  Size in bytes of the received HID report
 */
void CALLBACK_HID_Device_ProcessHIDReport(USB_ClassInfo_HID_Device_t* const HIDInterfaceInfo,
										  const uint8_t ReportID,
										  const uint8_t ReportType,
										  const void* ReportData,
										  const uint16_t ReportSize)
{
	uint8_t* LEDReport = (uint8_t*)ReportData;
	USB_LEDReport = *LEDReport;

}
