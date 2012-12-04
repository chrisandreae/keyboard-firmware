/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  Licensed under the GNU GPL v2 (see GPL2.txt).

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

#include <util/delay.h>     /* for _delay_ms() */
#include "kinesis.h"

#define KEY_NONE NO_KEY
// because the matrix is not tightly packed, we want a map from matrix
// position to logical key.
const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM = {
	/*      PIN:       5                        6                   21                  22               23                     24                     25                       26                     27                   28      */
	/*U4-Y0*/   { LOGICAL_KEY_KEYPAD,  LOGICAL_KEY_PROGRAM,  LOGICAL_KEY_F6,     LOGICAL_KEY_F3,  LOGICAL_KEY_ESC,      LOGICAL_KEY_F9,      LOGICAL_KEY_F12,           LOGICAL_KEY_PAUSE,     KEY_NONE,              KEY_NONE               }, // 0 0 0 0
	/*U4-Y1*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_F8,     LOGICAL_KEY_F5,  LOGICAL_KEY_F2,       LOGICAL_KEY_F11,     LOGICAL_KEY_SCROLLLOCK,    KEY_NONE,              KEY_NONE,              KEY_NONE               }, // 0 0 0 1
	/*U4-Y2*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_F7,     LOGICAL_KEY_F4,  LOGICAL_KEY_F1,       LOGICAL_KEY_F10,     LOGICAL_KEY_PRINTSCREEN,   KEY_NONE,              KEY_NONE,              KEY_NONE               }, // 0 0 1 0
	/*U4-Y3*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_5,      LOGICAL_KEY_T,   LOGICAL_KEY_G,        LOGICAL_KEY_B,       LOGICAL_KEY_RIGHT,         LOGICAL_KEY_L_ALT,     LOGICAL_KEY_L_CTRL,    KEY_NONE               }, // 0 0 1 1
	/*U4-Y4*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_4,      LOGICAL_KEY_R,   LOGICAL_KEY_F,        LOGICAL_KEY_V,       KEY_NONE,                  KEY_NONE,              LOGICAL_KEY_DELETE,    LOGICAL_KEY_R_ALT      }, // 0 1 0 0
	/*U4-Y5*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_3,      LOGICAL_KEY_E,   LOGICAL_KEY_D,        LOGICAL_KEY_C,       LOGICAL_KEY_LEFT,          LOGICAL_KEY_HOME,      LOGICAL_KEY_BACKSPACE, KEY_NONE               }, // 0 1 0 1
	/*U4-Y6*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_2,      LOGICAL_KEY_W,   LOGICAL_KEY_S,        LOGICAL_KEY_X,       LOGICAL_KEY_INTERNATIONAL, KEY_NONE,              LOGICAL_KEY_R_CTRL,    LOGICAL_KEY_PGUP       }, // 0 1 1 0
	/*U4-Y7*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_1,      LOGICAL_KEY_Q,   LOGICAL_KEY_A,        LOGICAL_KEY_Z,       LOGICAL_KEY_BACKTICK,      LOGICAL_KEY_END,       LOGICAL_KEY_ENTER,     KEY_NONE               }, // 0 1 1 1
	/*U5-Y0*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_EQUALS, LOGICAL_KEY_TAB, LOGICAL_KEY_CAPSLOCK, LOGICAL_KEY_LSHIFT,  KEY_NONE,                  KEY_NONE,              LOGICAL_KEY_SPACE,     LOGICAL_KEY_PGDN       }, // 1 0 0 0
	/*U5-Y1*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_UP,      LOGICAL_KEY_6,             LOGICAL_KEY_Y,         LOGICAL_KEY_H,         LOGICAL_KEY_N          }, // 1 0 0 1
	/*U5-Y2*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             KEY_NONE,            LOGICAL_KEY_7,             LOGICAL_KEY_U,         LOGICAL_KEY_J,         LOGICAL_KEY_M          }, // 1 0 1 0
	/*U5-Y3*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_DOWN,    LOGICAL_KEY_8,             LOGICAL_KEY_I,         LOGICAL_KEY_K,         LOGICAL_KEY_COMMA      }, // 1 0 1 1
	/*U5-Y4*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             KEY_NONE,            KEY_NONE,                  KEY_NONE,              KEY_NONE,              KEY_NONE               }, // 1 1 0 0
	/*U5-Y5*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_LSQUARE, LOGICAL_KEY_9,             LOGICAL_KEY_O,         LOGICAL_KEY_L,         LOGICAL_KEY_PERIOD     }, // 1 1 0 1
	/*U5-Y6*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_RSQUARE, LOGICAL_KEY_0,             LOGICAL_KEY_P,         LOGICAL_KEY_SEMICOLON, LOGICAL_KEY_SLASH      }, // 1 1 1 0
	/*U5-Y7*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             KEY_NONE,            LOGICAL_KEY_HYPHEN,        LOGICAL_KEY_BACKSLASH, LOGICAL_KEY_QUOTE,     LOGICAL_KEY_RSHIFT     }  // 1 1 1 1
};

#undef KEY_NONE


const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM = {
	SPECIAL_HID_KEY_KEYPAD,							   //	LOGICAL_KEY_KEYPAD
	SPECIAL_HID_KEY_PROGRAM,						   //	LOGICAL_KEY_PROGRAM
	// non-keypad layer
	HID_KEYBOARD_SC_A,								   //	LOGICAL_KEY_A
	HID_KEYBOARD_SC_B,								   //	LOGICAL_KEY_B
	HID_KEYBOARD_SC_C,								   //	LOGICAL_KEY_C
	HID_KEYBOARD_SC_D,								   //	LOGICAL_KEY_D
	HID_KEYBOARD_SC_E,								   //	LOGICAL_KEY_E
	HID_KEYBOARD_SC_F,								   //	LOGICAL_KEY_F
	HID_KEYBOARD_SC_G,								   //	LOGICAL_KEY_G
	HID_KEYBOARD_SC_H,								   //	LOGICAL_KEY_H
	HID_KEYBOARD_SC_I,								   //	LOGICAL_KEY_I
	HID_KEYBOARD_SC_J,								   //	LOGICAL_KEY_J
	HID_KEYBOARD_SC_K,								   //	LOGICAL_KEY_K
	HID_KEYBOARD_SC_L,								   //	LOGICAL_KEY_L
	HID_KEYBOARD_SC_M,								   //	LOGICAL_KEY_M
	HID_KEYBOARD_SC_N,								   //	LOGICAL_KEY_N
	HID_KEYBOARD_SC_O,								   //	LOGICAL_KEY_O
	HID_KEYBOARD_SC_P,								   //	LOGICAL_KEY_P
	HID_KEYBOARD_SC_Q,								   //	LOGICAL_KEY_Q
	HID_KEYBOARD_SC_R,								   //	LOGICAL_KEY_R
	HID_KEYBOARD_SC_S,								   //	LOGICAL_KEY_S
	HID_KEYBOARD_SC_T,								   //	LOGICAL_KEY_T
	HID_KEYBOARD_SC_U,								   //	LOGICAL_KEY_U
	HID_KEYBOARD_SC_V,								   //	LOGICAL_KEY_V
	HID_KEYBOARD_SC_W,								   //	LOGICAL_KEY_W
	HID_KEYBOARD_SC_X,								   //	LOGICAL_KEY_X
	HID_KEYBOARD_SC_Y,								   //	LOGICAL_KEY_Y
	HID_KEYBOARD_SC_Z,								   //	LOGICAL_KEY_Z
	HID_KEYBOARD_SC_1_AND_EXCLAMATION,				   //	LOGICAL_KEY_1
	HID_KEYBOARD_SC_2_AND_AT,						   //	LOGICAL_KEY_2
	HID_KEYBOARD_SC_3_AND_HASHMARK,					   //	LOGICAL_KEY_3
	HID_KEYBOARD_SC_4_AND_DOLLAR,					   //	LOGICAL_KEY_4
	HID_KEYBOARD_SC_5_AND_PERCENTAGE,				   //	LOGICAL_KEY_5
	HID_KEYBOARD_SC_6_AND_CARET,					   //	LOGICAL_KEY_6
	HID_KEYBOARD_SC_7_AND_AND_AMPERSAND,			   //	LOGICAL_KEY_7
	HID_KEYBOARD_SC_8_AND_ASTERISK,					   //	LOGICAL_KEY_8
	HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS,		   //	LOGICAL_KEY_9
	HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS,		   //	LOGICAL_KEY_0
	HID_KEYBOARD_SC_F1,								   //	LOGICAL_KEY_F1
	HID_KEYBOARD_SC_F2,								   //	LOGICAL_KEY_F2
	HID_KEYBOARD_SC_F3,								   //	LOGICAL_KEY_F3
	HID_KEYBOARD_SC_F4,								   //	LOGICAL_KEY_F4
	HID_KEYBOARD_SC_F5,								   //	LOGICAL_KEY_F5
	HID_KEYBOARD_SC_F6,								   //	LOGICAL_KEY_F6
	HID_KEYBOARD_SC_F7,								   //	LOGICAL_KEY_F7
	HID_KEYBOARD_SC_F8,								   //	LOGICAL_KEY_F8
	HID_KEYBOARD_SC_F9,								   //	LOGICAL_KEY_F9
	HID_KEYBOARD_SC_F10,							   //	LOGICAL_KEY_F10
	HID_KEYBOARD_SC_F11,							   //	LOGICAL_KEY_F11
	HID_KEYBOARD_SC_F12,							   //	LOGICAL_KEY_F12
	HID_KEYBOARD_SC_EQUAL_AND_PLUS,					   //	LOGICAL_KEY_EQUALS
	HID_KEYBOARD_SC_TAB,							   //	LOGICAL_KEY_TAB
	HID_KEYBOARD_SC_ESCAPE,							   //	LOGICAL_KEY_ESC
	HID_KEYBOARD_SC_CAPS_LOCK,						   //	LOGICAL_KEY_CAPSLOCK
	HID_KEYBOARD_SC_LEFT_SHIFT,						   //	LOGICAL_KEY_LSHIFT
	HID_KEYBOARD_SC_UP_ARROW,						   //	LOGICAL_KEY_UP
	HID_KEYBOARD_SC_DOWN_ARROW,						   //	LOGICAL_KEY_DOWN
	HID_KEYBOARD_SC_LEFT_ARROW,						   //	LOGICAL_KEY_LEFT
	HID_KEYBOARD_SC_RIGHT_ARROW,					   //	LOGICAL_KEY_RIGHT
	HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE, //	LOGICAL_KEY_LSQUARE
	HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE, //	LOGICAL_KEY_RSQUARE
	HID_KEYBOARD_SC_SCROLL_LOCK,					   //	LOGICAL_KEY_SCROLLLOCK
	HID_KEYBOARD_SC_PRINT_SCREEN,					   //	LOGICAL_KEY_PRINTSCREEN
	HID_KEYBOARD_SC_NON_US_BACKSLASH_AND_PIPE,		   //	LOGICAL_KEY_INTERNATIONAL
	HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE,			   //	LOGICAL_KEY_BACKTICK
	HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE,			   //	LOGICAL_KEY_HYPHEN
	HID_KEYBOARD_SC_PAUSE,							   //	LOGICAL_KEY_PAUSE
	HID_KEYBOARD_SC_END,							   //	LOGICAL_KEY_END
	HID_KEYBOARD_SC_HOME,							   //	LOGICAL_KEY_HOME
	HID_KEYBOARD_SC_DELETE,							   //	LOGICAL_KEY_DELETE
	HID_KEYBOARD_SC_BACKSPACE,						   //	LOGICAL_KEY_BACKSPACE
	HID_KEYBOARD_SC_LEFT_ALT,						   //	LOGICAL_KEY_L_ALT
	HID_KEYBOARD_SC_LEFT_CONTROL,					   //	LOGICAL_KEY_L_CTRL
	HID_KEYBOARD_SC_BACKSLASH_AND_PIPE,				   //	LOGICAL_KEY_BACKSLASH
	HID_KEYBOARD_SC_PAGE_DOWN,						   //	LOGICAL_KEY_PGDN
	HID_KEYBOARD_SC_PAGE_UP,						   //	LOGICAL_KEY_PGUP
	HID_KEYBOARD_SC_ENTER,							   //	LOGICAL_KEY_ENTER
	HID_KEYBOARD_SC_SPACE,							   //	LOGICAL_KEY_SPACE
	HID_KEYBOARD_SC_RIGHT_ALT,						   //	LOGICAL_KEY_R_ALT
	HID_KEYBOARD_SC_RIGHT_CONTROL,					   //	LOGICAL_KEY_R_CTRL
	HID_KEYBOARD_SC_SEMICOLON_AND_COLON,			   //	LOGICAL_KEY_SEMICOLON
	HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE,			   //	LOGICAL_KEY_QUOTE
	HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN,		   //	LOGICAL_KEY_COMMA
	HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN,		   //	LOGICAL_KEY_PERIOD
	HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK,		   //	LOGICAL_KEY_SLASH
	HID_KEYBOARD_SC_RIGHT_SHIFT,					   //	LOGICAL_KEY_RSHIFT
		/////////////////////////////////////////////////                              *** keypad mode default differs to base layer
	HID_KEYBOARD_SC_A,								   //  LOGICAL_KEY_KP_A,
	HID_KEYBOARD_SC_B,								   //  LOGICAL_KEY_KP_B,
	HID_KEYBOARD_SC_C,								   //  LOGICAL_KEY_KP_C,
	SPECIAL_HID_KEY_MOUSE_BACK,						   //  LOGICAL_KEY_KP_D,           ***
	SPECIAL_HID_KEY_MOUSE_FWD,						   //  LOGICAL_KEY_KP_E,           ***
	SPECIAL_HID_KEY_MOUSE_RIGHT,					   //  LOGICAL_KEY_KP_F,           ***
	HID_KEYBOARD_SC_G,								   //  LOGICAL_KEY_KP_G,
	HID_KEYBOARD_SC_H,								   //  LOGICAL_KEY_KP_H,
	HID_KEYBOARD_SC_KEYPAD_8_AND_UP_ARROW,			   //  LOGICAL_KEY_KP_I,		   ***
	HID_KEYBOARD_SC_KEYPAD_4_AND_LEFT_ARROW,		   //  LOGICAL_KEY_KP_J,		   ***
	HID_KEYBOARD_SC_KEYPAD_5,						   //  LOGICAL_KEY_KP_K,		   ***
	HID_KEYBOARD_SC_KEYPAD_6_AND_RIGHT_ARROW,		   //  LOGICAL_KEY_KP_L,           ***
	HID_KEYBOARD_SC_KEYPAD_1_AND_END,				   //  LOGICAL_KEY_KP_M,		   ***
	HID_KEYBOARD_SC_N,								   //  LOGICAL_KEY_KP_N,
	HID_KEYBOARD_SC_KEYPAD_9_AND_PAGE_UP,			   //  LOGICAL_KEY_KP_O,		   ***
	HID_KEYBOARD_SC_KEYPAD_MINUS,					   //  LOGICAL_KEY_KP_P,		   ***
	HID_KEYBOARD_SC_Q,								   //  LOGICAL_KEY_KP_Q,
	HID_KEYBOARD_SC_R,								   //  LOGICAL_KEY_KP_R,
	SPECIAL_HID_KEY_MOUSE_LEFT,						   //  LOGICAL_KEY_KP_S,           ***
	HID_KEYBOARD_SC_T,								   //  LOGICAL_KEY_KP_T,
	HID_KEYBOARD_SC_KEYPAD_7_AND_HOME,				   //  LOGICAL_KEY_KP_U,		   ***
	HID_KEYBOARD_SC_V,								   //  LOGICAL_KEY_KP_V,
	HID_KEYBOARD_SC_W,								   //  LOGICAL_KEY_KP_W,
	HID_KEYBOARD_SC_X,								   //  LOGICAL_KEY_KP_X,
	HID_KEYBOARD_SC_Y,								   //  LOGICAL_KEY_KP_Y,
	HID_KEYBOARD_SC_Z,								   //  LOGICAL_KEY_KP_Z,
	HID_KEYBOARD_SC_1_AND_EXCLAMATION,				   //  LOGICAL_KEY_KP_1,
	HID_KEYBOARD_SC_2_AND_AT,						   //  LOGICAL_KEY_KP_2,
	HID_KEYBOARD_SC_3_AND_HASHMARK,					   //  LOGICAL_KEY_KP_3,
	HID_KEYBOARD_SC_4_AND_DOLLAR,					   //  LOGICAL_KEY_KP_4,
	HID_KEYBOARD_SC_5_AND_PERCENTAGE,				   //  LOGICAL_KEY_KP_5,
	HID_KEYBOARD_SC_6_AND_CARET,					   //  LOGICAL_KEY_KP_6,
	HID_KEYBOARD_SC_NUM_LOCK,						   //  LOGICAL_KEY_KP_7,		   ***
	HID_KEYBOARD_SC_EQUAL_AND_PLUS,					   //  LOGICAL_KEY_KP_8,		   *** // _SC_KEYPAD_EQUAL_SIGN (0x86) is ignored by windows, other docs say kp= is 0x67
	HID_KEYBOARD_SC_KEYPAD_SLASH,					   //  LOGICAL_KEY_KP_9,		   ***
	HID_KEYBOARD_SC_KEYPAD_ASTERISK,				   //  LOGICAL_KEY_KP_0,		   ***
	HID_KEYBOARD_SC_F1,								   //  LOGICAL_KEY_KP_F1,
	HID_KEYBOARD_SC_F2,								   //  LOGICAL_KEY_KP_F2,
	HID_KEYBOARD_SC_F3,								   //  LOGICAL_KEY_KP_F3,
	HID_KEYBOARD_SC_F4,								   //  LOGICAL_KEY_KP_F4,
	HID_KEYBOARD_SC_F5,								   //  LOGICAL_KEY_KP_F5,
	HID_KEYBOARD_SC_F6,								   //  LOGICAL_KEY_KP_F6,
	HID_KEYBOARD_SC_F7,								   //  LOGICAL_KEY_KP_F7,
	HID_KEYBOARD_SC_F8,								   //  LOGICAL_KEY_KP_F8,
	HID_KEYBOARD_SC_F9,								   //  LOGICAL_KEY_KP_F9,
	HID_KEYBOARD_SC_F10,							   //  LOGICAL_KEY_KP_F10,
	HID_KEYBOARD_SC_F11,							   //  LOGICAL_KEY_KP_F11,
	HID_KEYBOARD_SC_F12,							   //  LOGICAL_KEY_KP_F12,
	HID_KEYBOARD_SC_EQUAL_AND_PLUS,					   //  LOGICAL_KEY_KP_EQUALS,
	HID_KEYBOARD_SC_TAB,							   //  LOGICAL_KEY_KP_TAB,
	HID_KEYBOARD_SC_ESCAPE,							   //  LOGICAL_KEY_KP_ESC,
	NO_KEY,											   //  LOGICAL_KEY_KP_CAPSLOCK,    ***
	HID_KEYBOARD_SC_LEFT_SHIFT,						   //  LOGICAL_KEY_KP_LSHIFT,
	HID_KEYBOARD_SC_UP_ARROW,						   //  LOGICAL_KEY_KP_UP,
	HID_KEYBOARD_SC_DOWN_ARROW,						   //  LOGICAL_KEY_KP_DOWN,
	HID_KEYBOARD_SC_LEFT_ARROW,						   //  LOGICAL_KEY_KP_LEFT,
	HID_KEYBOARD_SC_RIGHT_ARROW,					   //  LOGICAL_KEY_KP_RIGHT,
	HID_KEYBOARD_SC_KEYPAD_DOT_AND_DELETE,			   //  LOGICAL_KEY_KP_LSQUARE,	   ***
	HID_KEYBOARD_SC_KEYPAD_ENTER,					   //  LOGICAL_KEY_KP_RSQUARE,	   ***
	HID_KEYBOARD_SC_RIGHT_GUI,						   //  LOGICAL_KEY_KP_SCROLLLOCK,  ***
	HID_KEYBOARD_SC_LEFT_GUI,						   //  LOGICAL_KEY_KP_PRINTSCREEN, ***
	HID_KEYBOARD_SC_INSERT,							   //  LOGICAL_KEY_KP_INTERNATIONAL, ***
	HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE,			   //  LOGICAL_KEY_KP_BACKTICK,
	HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE,			   //  LOGICAL_KEY_KP_HYPHEN,
	0x65,											   //  LOGICAL_KEY_KP_PAUSE,	   ***   // this seems to be the windows context menu key. It's not in the LUFA header.
	SPECIAL_HID_KEY_MOUSE_BTN5,						   //  LOGICAL_KEY_KP_END,         ***
	SPECIAL_HID_KEY_MOUSE_BTN4,						   //  LOGICAL_KEY_KP_HOME,        ***
	SPECIAL_HID_KEY_MOUSE_BTN2,						   //  LOGICAL_KEY_KP_DELETE,      ***
	SPECIAL_HID_KEY_MOUSE_BTN1,						   //  LOGICAL_KEY_KP_BACKSPACE,   ***
	HID_KEYBOARD_SC_LEFT_ALT,						   //  LOGICAL_KEY_KP_L_ALT
	SPECIAL_HID_KEY_MOUSE_BTN3,						   //  LOGICAL_KEY_KP_L_CTRL       ***
	HID_KEYBOARD_SC_BACKSLASH_AND_PIPE,				   //  LOGICAL_KEY_KP_BACKSLASH,
	HID_KEYBOARD_SC_PAGE_DOWN,						   //  LOGICAL_KEY_KP_PGDN,
	HID_KEYBOARD_SC_PAGE_UP,						   //  LOGICAL_KEY_KP_PGUP,
	HID_KEYBOARD_SC_ENTER,							   //  LOGICAL_KEY_KP_ENTER,
	HID_KEYBOARD_SC_KEYPAD_0_AND_INSERT,			   //  LOGICAL_KEY_KP_SPACE,	   ***
	HID_KEYBOARD_SC_RIGHT_ALT,						   //  LOGICAL_KEY_KP_R_ALT,
	HID_KEYBOARD_SC_RIGHT_CONTROL,					   //  LOGICAL_KEY_KP_R_CTRL,
	HID_KEYBOARD_SC_KEYPAD_PLUS,					   //  LOGICAL_KEY_KP_SEMICOLON,   ***
	HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE,			   //  LOGICAL_KEY_KP_QUOTE,
	HID_KEYBOARD_SC_KEYPAD_2_AND_DOWN_ARROW,		   //  LOGICAL_KEY_KP_COMMA,	   ***
	HID_KEYBOARD_SC_KEYPAD_3_AND_PAGE_DOWN,			   //  LOGICAL_KEY_KP_PERIOD,	   ***
	HID_KEYBOARD_SC_KEYPAD_ENTER,					   //  LOGICAL_KEY_KP_SLASH,	   ***
	HID_KEYBOARD_SC_RIGHT_SHIFT,					   //  LOGICAL_KEY_KP_RSHIFT,
};

void ports_init(void){
	// Set up input: port C7 and all of port B
	// we want to enable internal pull-ups on all of these pins - the multiplexer (or whatever else in the case of 5 and 6)
	// will pull its selected line low, and therefore if we test low, selected key is pressed, high, key is not pressed.
	INPUT_PIN5_DDR  &= ~INPUT_PIN5; // 0 = input pin
	INPUT_PIN5_PORT |=  INPUT_PIN5; // 1 = pull-up enabled
	INPUT_PIN6_DDR  &= ~INPUT_PIN6;
	INPUT_PIN6_PORT |=  INPUT_PIN6;
	INPUT_REST_DDR  &=  ~INPUT_REST;
	INPUT_REST_PORT |=   INPUT_REST;

	// Set up matrix selector output - C4, C5, C6.
	// this has an external pull-up, so we cause a low value by being a current sink, and a high by being high-impedence.
	// this corresponds to output low (PORT=0, DDR=1) or to input with no pullup (PORT=0, DDR=0)
	MATRIX_PORT &= ~MATRIX_MASK;
	MATRIX_DDR  |=  MATRIX_MASK; // start as output-low (sink current)

	// Set up LEDs - they're externally pulled up, so output-low(1,0) to enable, input-highz(0,0) to disable.
	LED_PORT &= ~(ALL_LEDS);
	LED_DDR  &= ~(ALL_LEDS); // start as hi-z (disabled)

#if USE_BUZZER
	// start out output/low
	BUZZER_PORT &= ~BUZZER;
	BUZZER_DDR  |= BUZZER;
#endif

#ifdef BITBANG_TWI
	// Serial eeprom lines have external pull-ups, so 0 = output-low(1,0) / 1 = input-highz(0,0)
	EEPROM_PORT &= ~(EEPROM_SCL | EEPROM_SDA); // initially leave floating
	EEPROM_DDR  &= ~(EEPROM_SCL | EEPROM_SDA);
#else
	TWSR = 0x00;
	TWBR = 72; // 100kHz SCL clock (The eeprom starts responding
			   // poorly (returns 0xff) when faster. Can go up to
			   // 200kHz (TWBR=32) on the all-in-one board design.)

	//enable TWI
	TWCR = (1<<TWEN);
#endif // bitbang

}


void matrix_select_row(uint8_t matrix_row){
	// Select output using four bits starting at MATRIX_SELECT_A
	// set output with MATRIX_DDR - 0 means high (high-z input, external pullup), 1 means low (output low, current sink)

	uint8_t output_ddr_val = MATRIX_MASK; // start with all low level (current sink - 1)
	if(matrix_row & 0x1){
		output_ddr_val &= ~MATRIX_SELECT_A; // set DDR bit 0 for high level
	}
	if(matrix_row & 0x2){
		output_ddr_val &= ~MATRIX_SELECT_B;
	}
	if(matrix_row & 0x4){
		output_ddr_val &= ~MATRIX_SELECT_C;
	}
	if(matrix_row & 0x8){
		output_ddr_val &= ~MATRIX_SELECT_P138SEL;
	}

	MATRIX_DDR = (MATRIX_DDR & ~MATRIX_MASK) | output_ddr_val;
}


uint8_t matrix_read_column(uint8_t matrix_column){
	/*
	if(matrix_column >= MATRIX_COLS){
		panic();
	}
	*/

	if(matrix_column == 0){
		// read pin5
		uint8_t val = (INPUT_PIN5_PIN & INPUT_PIN5) == 0; // if high, button not pressed, if low, button pressed
		return val;
	}
	else if(matrix_column == 1){
		// read pin6
		uint8_t val = (INPUT_PIN6_PIN & INPUT_PIN6) == 0; // if high, button not pressed, if low, button pressed
		return val;
	}
	else{
		uint8_t shift = matrix_column - 2;
		uint8_t val = (INPUT_REST_PIN & (1<<shift)) == 0;
		return val;
	}
}

void set_all_leds(uint8_t led_mask){
	led_mask &= ALL_LEDS; // only touch within led range

	LED_DDR = (LED_DDR & ~ALL_LEDS) | led_mask;
}

void test_leds(void){
	for(int i = 0; i < 10; ++i){
		set_all_leds(0);
		_delay_ms(500);
		set_all_leds(LED_CAPS);
		_delay_ms(500);
		set_all_leds(LED_NUMLOCK);
		_delay_ms(500);
		set_all_leds(LED_SCROLLLOCK);
		_delay_ms(500);
		set_all_leds(LED_KEYPAD);
		_delay_ms(500);
	}
}
