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
#include "ergodox.h"
#include "twi.h"

#define KEY_NONE NO_KEY
// because the matrix is not tightly packed, we want a map from matrix
// position to logical key. (Ergodox matrix names in parens)
const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM = {
	//                  PF0 (5)                PF1 (4)              PF4 (3)                PF5 (2)              PF6 (1)            PF7 (0)         |   GPB0 (5)   GPB1 (4)   GPB2 (3)   GPB3 (2)  GPB4 (1)  GPB5 (0)
	/*PB0 (7) + GPA0*/ {LOGICAL_KEY_PROGRAM,   LOGICAL_KEY_RCOL2_1, NO_KEY,                LOGICAL_KEY_RCOL2_2, NO_KEY,            LOGICAL_KEY_R_ALT,  LOGICAL_KEY_LCOL1_1, LOGICAL_KEY_LCOL1_2, LOGICAL_KEY_LCOL1_3, LOGICAL_KEY_LCOL1_4, LOGICAL_KEY_LROW1, NO_KEY},
	/*PB1 (8) + GPA1*/ {LOGICAL_KEY_6,         LOGICAL_KEY_Y,       LOGICAL_KEY_H,         LOGICAL_KEY_N,       NO_KEY,            LOGICAL_KEY_R_CTRL, LOGICAL_KEY_1,       LOGICAL_KEY_Q,       LOGICAL_KEY_A, LOGICAL_KEY_Z, LOGICAL_KEY_LROW2, LOGICAL_KEY_END},
	/*PB2 (9) + GPA2*/ {LOGICAL_KEY_7,         LOGICAL_KEY_U,       LOGICAL_KEY_J,         LOGICAL_KEY_M,       LOGICAL_KEY_RROW5, LOGICAL_KEY_PGUP,   LOGICAL_KEY_2,       LOGICAL_KEY_W,       LOGICAL_KEY_S, LOGICAL_KEY_X, LOGICAL_KEY_LROW3, LOGICAL_KEY_DELETE},
	/*PB3 (A) + GPA3*/ {LOGICAL_KEY_8,         LOGICAL_KEY_I,       LOGICAL_KEY_K,         LOGICAL_KEY_COMMA,   LOGICAL_KEY_RROW4, LOGICAL_KEY_SPACE,  LOGICAL_KEY_3,       LOGICAL_KEY_E,       LOGICAL_KEY_D, LOGICAL_KEY_C, LOGICAL_KEY_LROW4, LOGICAL_KEY_BACKSPACE},
	/*PD2 (B) + GPA4*/ {LOGICAL_KEY_9,         LOGICAL_KEY_O,       LOGICAL_KEY_L,         LOGICAL_KEY_PERIOD,  LOGICAL_KEY_RROW3, LOGICAL_KEY_ENTER,  LOGICAL_KEY_4,       LOGICAL_KEY_R,       LOGICAL_KEY_F, LOGICAL_KEY_V, LOGICAL_KEY_LROW5, LOGICAL_KEY_HOME},
	/*PD3 (C) + GPA5*/ {LOGICAL_KEY_0,         LOGICAL_KEY_P,       LOGICAL_KEY_SEMICOLON, LOGICAL_KEY_SLASH,   LOGICAL_KEY_RROW2, LOGICAL_KEY_PGDN,   LOGICAL_KEY_5,       LOGICAL_KEY_T,       LOGICAL_KEY_G, LOGICAL_KEY_B, NO_KEY,            LOGICAL_KEY_L_CTRL},
	/*PC6 (D) + GPA6*/ {LOGICAL_KEY_RCOL1_1,   LOGICAL_KEY_RCOL1_2, LOGICAL_KEY_RCOL1_3,   LOGICAL_KEY_RCOL1_4, LOGICAL_KEY_RROW1, NO_KEY,             LOGICAL_KEY_KEYPAD,  LOGICAL_KEY_LCOL2_1, NO_KEY,  LOGICAL_KEY_LCOL2_2, NO_KEY,            LOGICAL_KEY_L_ALT}
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
	HID_KEYBOARD_SC_SEMICOLON_AND_COLON,			   //	LOGICAL_KEY_SEMICOLON
	HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN,		   //	LOGICAL_KEY_COMMA
	HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN,		   //	LOGICAL_KEY_PERIOD
	HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK,		   //	LOGICAL_KEY_SLASH
    // LHS extra keys
	HID_KEYBOARD_SC_EQUAL_AND_PLUS,					   //	LOGICAL_KEY_LCOL1_1, // outer column
	HID_KEYBOARD_SC_TAB, 							   //	LOGICAL_KEY_LCOL1_2,
	HID_KEYBOARD_SC_CAPS_LOCK,						   //	LOGICAL_KEY_LCOL1_3,
	HID_KEYBOARD_SC_LEFT_SHIFT,						   //	LOGICAL_KEY_LCOL1_4,
	HID_KEYBOARD_SC_LEFT_GUI,  						   //	LOGICAL_KEY_LROW1, // bottom row
	HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE,			   //	LOGICAL_KEY_LROW2,
	HID_KEYBOARD_SC_NON_US_BACKSLASH_AND_PIPE, 		   //	LOGICAL_KEY_LROW3,
	HID_KEYBOARD_SC_LEFT_ARROW,						   //	LOGICAL_KEY_LROW4,
	HID_KEYBOARD_SC_RIGHT_ARROW,					   //	LOGICAL_KEY_LROW5,
	NO_KEY,											   //	LOGICAL_KEY_LCOL2_1, // inner column (top-most keys in this column are pgm/kpd)
	NO_KEY,											   //	LOGICAL_KEY_LCOL2_2,
	// Right hand extra keys
	HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE,			   //	LOGICAL_KEY_RCOL1_1, // outer column
	HID_KEYBOARD_SC_BACKSLASH_AND_PIPE,				   //	LOGICAL_KEY_RCOL1_2,
	HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE, 			   //	LOGICAL_KEY_RCOL1_3,
	HID_KEYBOARD_SC_RIGHT_SHIFT,					   //	LOGICAL_KEY_RCOL1_4,
	HID_KEYBOARD_SC_RIGHT_GUI,						   //	LOGICAL_KEY_RROW1, // bottom row
	HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE, //	LOGICAL_KEY_RROW2,
	HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE, //	LOGICAL_KEY_RROW3,
	HID_KEYBOARD_SC_DOWN_ARROW, 					   //	LOGICAL_KEY_RROW4,
	HID_KEYBOARD_SC_UP_ARROW, 						   //	LOGICAL_KEY_RROW5,
	NO_KEY, 										   //	LOGICAL_KEY_RCOL2_1, // inner column
	NO_KEY, 										   //	LOGICAL_KEY_RCOL2_2,
	// Left hand thumbpad
	HID_KEYBOARD_SC_LEFT_ALT, 						   //	LOGICAL_KEY_L_ALT,
	HID_KEYBOARD_SC_LEFT_CONTROL,					   //	LOGICAL_KEY_L_CTRL,
	HID_KEYBOARD_SC_HOME,							   //	LOGICAL_KEY_HOME,
	HID_KEYBOARD_SC_END,							   //	LOGICAL_KEY_END,
	HID_KEYBOARD_SC_BACKSPACE,						   //	LOGICAL_KEY_BACKSPACE,
	HID_KEYBOARD_SC_DELETE,							   //	LOGICAL_KEY_DELETE,
	// Right hand thumb pad
	HID_KEYBOARD_SC_RIGHT_ALT,						   //	LOGICAL_KEY_R_ALT,
	HID_KEYBOARD_SC_RIGHT_CONTROL,					   //	LOGICAL_KEY_R_CTRL,
	HID_KEYBOARD_SC_PAGE_DOWN,						   //	LOGICAL_KEY_PGDN,
	HID_KEYBOARD_SC_PAGE_UP,						   //	LOGICAL_KEY_PGUP,
	HID_KEYBOARD_SC_ENTER,							   //	LOGICAL_KEY_ENTER,
	HID_KEYBOARD_SC_SPACE,							   //	LOGICAL_KEY_SPACE,

		/////////////////////////////////////////////////              *** keypad mode default differs to base layer

	HID_KEYBOARD_SC_A,								   //	LOGICAL_KEY_KP_A
	HID_KEYBOARD_SC_B,								   //	LOGICAL_KEY_KP_B
	HID_KEYBOARD_SC_C,								   //	LOGICAL_KEY_KP_C
	SPECIAL_HID_KEY_MOUSE_BACK, 					   //	LOGICAL_KEY_KP_D ***
	SPECIAL_HID_KEY_MOUSE_FWD, 						   //	LOGICAL_KEY_KP_E ***
	SPECIAL_HID_KEY_MOUSE_RIGHT, 					   //	LOGICAL_KEY_KP_F ***
	HID_KEYBOARD_SC_G,								   //	LOGICAL_KEY_KP_G
	HID_KEYBOARD_SC_H,								   //	LOGICAL_KEY_KP_H
	HID_KEYBOARD_SC_KEYPAD_8_AND_UP_ARROW,			   //	LOGICAL_KEY_KP_I ***
	HID_KEYBOARD_SC_KEYPAD_4_AND_LEFT_ARROW,		   //	LOGICAL_KEY_KP_J ***
	HID_KEYBOARD_SC_KEYPAD_5,						   //	LOGICAL_KEY_KP_K ***
	HID_KEYBOARD_SC_KEYPAD_6_AND_RIGHT_ARROW,		   //	LOGICAL_KEY_KP_L ***
	HID_KEYBOARD_SC_KEYPAD_1_AND_END,				   //	LOGICAL_KEY_KP_M ***
	HID_KEYBOARD_SC_N,								   //	LOGICAL_KEY_KP_N
	HID_KEYBOARD_SC_KEYPAD_9_AND_PAGE_UP,			   //	LOGICAL_KEY_KP_O ***
	HID_KEYBOARD_SC_KEYPAD_MINUS,					   //	LOGICAL_KEY_KP_P ***
	HID_KEYBOARD_SC_Q,								   //	LOGICAL_KEY_KP_Q
	HID_KEYBOARD_SC_R,								   //	LOGICAL_KEY_KP_R
	SPECIAL_HID_KEY_MOUSE_LEFT,						   //	LOGICAL_KEY_KP_S ***
	HID_KEYBOARD_SC_T,								   //	LOGICAL_KEY_KP_T
	HID_KEYBOARD_SC_KEYPAD_7_AND_HOME,				   //	LOGICAL_KEY_KP_U ***
	HID_KEYBOARD_SC_V,								   //	LOGICAL_KEY_KP_V
	HID_KEYBOARD_SC_W,								   //	LOGICAL_KEY_KP_W
	HID_KEYBOARD_SC_X,								   //	LOGICAL_KEY_KP_X
	HID_KEYBOARD_SC_Y,								   //	LOGICAL_KEY_KP_Y
	HID_KEYBOARD_SC_Z,								   //	LOGICAL_KEY_KP_Z
	HID_KEYBOARD_SC_1_AND_EXCLAMATION,				   //	LOGICAL_KEY_KP_1
	HID_KEYBOARD_SC_2_AND_AT,						   //	LOGICAL_KEY_KP_2
	HID_KEYBOARD_SC_3_AND_HASHMARK,					   //	LOGICAL_KEY_KP_3
	HID_KEYBOARD_SC_4_AND_DOLLAR,					   //	LOGICAL_KEY_KP_4
	HID_KEYBOARD_SC_5_AND_PERCENTAGE,				   //	LOGICAL_KEY_KP_5
	HID_KEYBOARD_SC_6_AND_CARET,					   //	LOGICAL_KEY_KP_6
	HID_KEYBOARD_SC_NUM_LOCK,						   //	LOGICAL_KEY_KP_7 ***
	HID_KEYBOARD_SC_EQUAL_AND_PLUS,					   //	LOGICAL_KEY_KP_8 ***
	HID_KEYBOARD_SC_KEYPAD_SLASH,					   //	LOGICAL_KEY_KP_9 ***
	HID_KEYBOARD_SC_KEYPAD_ASTERISK,				   //	LOGICAL_KEY_KP_0 ***
	HID_KEYBOARD_SC_SEMICOLON_AND_COLON,			   //	LOGICAL_KEY_KP_SEMICOLON
	HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN,		   //	LOGICAL_KEY_KP_COMMA
	HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN,		   //	LOGICAL_KEY_KP_PERIOD
	HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK,		   //	LOGICAL_KEY_KP_SLASH
    // LHS extra keys
	HID_KEYBOARD_SC_EQUAL_AND_PLUS,					   //	LOGICAL_KEY_KP_LCOL1_1, // outer column
	HID_KEYBOARD_SC_TAB, 							   //	LOGICAL_KEY_KP_LCOL1_2,
	HID_KEYBOARD_SC_CAPS_LOCK,						   //	LOGICAL_KEY_KP_LCOL1_3,
	HID_KEYBOARD_SC_LEFT_SHIFT,						   //	LOGICAL_KEY_KP_LCOL1_4,
	HID_KEYBOARD_SC_LEFT_GUI,  						   //	LOGICAL_KEY_KP_LROW1, // bottom row
	HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE,			   //	LOGICAL_KEY_KP_LROW2,
	HID_KEYBOARD_SC_NON_US_BACKSLASH_AND_PIPE, 		   //	LOGICAL_KEY_KP_LROW3,
	HID_KEYBOARD_SC_LEFT_ARROW,						   //	LOGICAL_KEY_KP_LROW4,
	HID_KEYBOARD_SC_RIGHT_ARROW,					   //	LOGICAL_KEY_KP_LROW5,
	NO_KEY,											   //	LOGICAL_KEY_KP_LCOL2_1, // inner column (top-most keys in this column are pgm/kpd)
	NO_KEY,											   //	LOGICAL_KEY_KP_LCOL2_2,
	// Right hand extra keys
	HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE,			   //	LOGICAL_KEY_KP_RCOL1_1, // outer column
	HID_KEYBOARD_SC_BACKSLASH_AND_PIPE,				   //	LOGICAL_KEY_KP_RCOL1_2,
	HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE, 			   //	LOGICAL_KEY_KP_RCOL1_3,
	HID_KEYBOARD_SC_RIGHT_SHIFT,					   //	LOGICAL_KEY_KP_RCOL1_4,
	HID_KEYBOARD_SC_RIGHT_GUI,						   //	LOGICAL_KEY_KP_RROW1, // bottom row
	HID_KEYBOARD_SC_KEYPAD_ENTER, 					   //	LOGICAL_KEY_KP_RROW2, ***
	HID_KEYBOARD_SC_KEYPAD_DOT_AND_DELETE, 			   //	LOGICAL_KEY_KP_RROW3, ***
	HID_KEYBOARD_SC_DOWN_ARROW, 					   //	LOGICAL_KEY_KP_RROW4,
	HID_KEYBOARD_SC_UP_ARROW, 						   //	LOGICAL_KEY_KP_RROW5,
	NO_KEY, 										   //	LOGICAL_KEY_KP_RCOL2_1, // inner column
	NO_KEY, 										   //	LOGICAL_KEY_KP_RCOL2_2,
	// Left hand thumbpad
	HID_KEYBOARD_SC_LEFT_ALT, 						   //	LOGICAL_KEY_KP_L_ALT,
	SPECIAL_HID_KEY_MOUSE_BTN3,						   //	LOGICAL_KEY_KP_L_CTRL, ***
	SPECIAL_HID_KEY_MOUSE_BTN4, 					   //	LOGICAL_KEY_KP_HOME, ***
	SPECIAL_HID_KEY_MOUSE_BTN5, 					   //	LOGICAL_KEY_KP_END, ***
	SPECIAL_HID_KEY_MOUSE_BTN1, 					   //	LOGICAL_KEY_KP_BACKSPACE, ***
	SPECIAL_HID_KEY_MOUSE_BTN2, 					   //	LOGICAL_KEY_KP_DELETE, ***
	// Right hand thumbpad
	HID_KEYBOARD_SC_RIGHT_ALT,						   //	LOGICAL_KEY_KP_R_ALT,
	HID_KEYBOARD_SC_RIGHT_CONTROL,					   //	LOGICAL_KEY_KP_R_CTRL,
	HID_KEYBOARD_SC_PAGE_DOWN,						   //	LOGICAL_KEY_KP_PGDN,
	HID_KEYBOARD_SC_PAGE_UP,						   //	LOGICAL_KEY_KP_PGUP,
	HID_KEYBOARD_SC_ENTER,							   //	LOGICAL_KEY_KP_ENTER,
	HID_KEYBOARD_SC_SPACE,							   //	LOGICAL_KEY_KP_SPACE,
};

#define twi_write_byte_checked(x) if(ACK != twi_write_byte(x)) goto err;

static uint8_t init_mcp23018(){
	// Set up IO direction
	// Rows (output direction) are GPA 0-6
	// Columns (input direction) are GPB0-5
	twi_start();
	twi_write_byte_checked(MCP23018_ADDR | MCP23018_WRITE);
	twi_write_byte(MCP23018_IODIRA);
	twi_write_byte(0b10000000); // A0-6 output
	twi_stop(WAIT);

	// set up pull-up registers on input columns
	twi_start();
	twi_write_byte_checked(MCP23018_ADDR | MCP23018_WRITE);
	twi_write_byte(MCP23018_GPPUB);
	twi_write_byte(0b00111111); // pull-ups on on inputs
	twi_stop(WAIT);

	// set up outputs to high-z
	twi_start();
	twi_write_byte_checked(MCP23018_ADDR | MCP23018_WRITE);
	twi_write_byte(MCP23018_OLATA);
	twi_write_byte(0b01111111);
	twi_stop(WAIT);

	return 1;
 err:
	PORTD |= (1<<6); // signal error by lighting up the internal LED
	twi_stop(NOWAIT);
	return 0;
}

void ports_init(void){
	// Set up input
	// we want to enable internal pull-ups on all of these pins: we're scanning by pulling low.
	RIGHT_MATRIX_IN_DDR  &= ~RIGHT_MATRIX_IN_MASK; // 0 = input pin
	RIGHT_MATRIX_IN_PORT |=  RIGHT_MATRIX_IN_MASK; // 1 = pull-up enabled

	// Set up matrix scan: initialize all as high-impedence
	RIGHT_MATRIX_OUT_1_DDR  &= ~RIGHT_MATRIX_OUT_1_MASK; // 0 = input
	RIGHT_MATRIX_OUT_1_PORT &= ~RIGHT_MATRIX_OUT_1_MASK; // 0 = high-z

	RIGHT_MATRIX_OUT_2_DDR  &= ~RIGHT_MATRIX_OUT_2_MASK; // 0 = input
	RIGHT_MATRIX_OUT_2_PORT &= ~RIGHT_MATRIX_OUT_2_MASK; // 0 = high-z

	RIGHT_MATRIX_OUT_3_DDR  &= ~RIGHT_MATRIX_OUT_3_MASK; // 0 = input
	RIGHT_MATRIX_OUT_3_PORT &= ~RIGHT_MATRIX_OUT_3_MASK; // 0 = high-z

	// Set up LEDs. I think we drive them directly: so output-high to enable (1,1), output-low (1,0) to disable.
	LED_DDR  |=   ALL_LEDS; // start as 1,0
	LED_PORT &= ~(ALL_LEDS);

#if USE_BUZZER
	// start out output/low
	BUZZER_PORT &= ~BUZZER;
	BUZZER_DDR  |= BUZZER;
#endif

	twi_init();

	//enable TWI
	TWCR = (1<<TWEN);

	// Set up the internal LED
	DDRD  |= (1<<6);  // output
	PORTD &= ~(1<<6); // off

	// initialize the MCP23018
	init_mcp23018();

		// turn on the internal LED to debug
		/* if(matrix_row == 1) PORTD |= (1<<6); */
		/* else PORTD &= ~(1<<6); */
}

// Init high
static uint8_t cached_mcp_columns = 0b00111111;

void matrix_select_row(uint8_t matrix_row){
	// Set on right hand side

	// Reset all matrix rows to input (high-z)
	RIGHT_MATRIX_OUT_1_DDR  &= ~RIGHT_MATRIX_OUT_1_MASK; // 0 = input
	RIGHT_MATRIX_OUT_2_DDR  &= ~RIGHT_MATRIX_OUT_2_MASK;
	RIGHT_MATRIX_OUT_3_DDR  &= ~RIGHT_MATRIX_OUT_3_MASK;

	// Set the selected row to output
	if(matrix_row < 4){
		RIGHT_MATRIX_OUT_1_DDR |= (1 << matrix_row);
	}
	else if(matrix_row < 6){
		RIGHT_MATRIX_OUT_2_DDR |= (RIGHT_MATRIX_OUT_2_START << (matrix_row - 4));
	}
	else if(matrix_row == 6){
		RIGHT_MATRIX_OUT_3_DDR |= RIGHT_MATRIX_OUT_3_MASK;
	}

	// Set on left hand side
	if(matrix_row == 0){
		init_mcp23018(); // reinitialize the MCP23018: it may have been unplugged
	}

	twi_start();
	twi_write_byte_checked(MCP23018_ADDR | MCP23018_WRITE);
	twi_write_byte(MCP23018_GPIOA);
	twi_write_byte(0xFF & ~(1 << matrix_row));

	twi_start();
	twi_write_byte_checked(MCP23018_ADDR | MCP23018_READ);
	cached_mcp_columns = twi_read_byte(NACK);
	twi_stop(WAIT);

	return;
 err:
	PORTD |= (1<<6); // signal error by lighting up the internal LED
	twi_stop(NOWAIT);
}

uint8_t matrix_read_column(uint8_t matrix_column){
	if(matrix_column < 6){
		// Right hand side
		uint8_t shift = matrix_column;
		if(shift > 1){
			shift += 2; // Handle hole in input port
		}
		uint8_t val = (RIGHT_MATRIX_IN_PIN & (1<<shift)) == 0;
		return val;
	}
	else{
		uint8_t shift = matrix_column - 6;
		uint8_t val = (cached_mcp_columns & (1<<shift)) == 0;
		/* if(matrix_column == 7) */
			return val; // temporarily: this column includes 'q' if row 2
		/* else */
			/* return 0; */
	}
}


void set_all_leds(uint8_t led_mask){
	led_mask &= ALL_LEDS; // only touch within led range

	LED_PORT = (LED_DDR & ~ALL_LEDS) | led_mask;
}

void test_leds(void){
	for(int i = 0; i < 10; ++i){
		set_all_leds(0);
		_delay_ms(500);
		set_all_leds(LED_CAPS);
		_delay_ms(500);
		set_all_leds(LED_NUMLOCK);
		_delay_ms(500);
		set_all_leds(LED_KEYPAD);
		_delay_ms(500);
	}
}
