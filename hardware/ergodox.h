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

#ifndef __ERGODOX_H
#define __ERGODOX_H

#include "keystate.h"

// Continue using the same USB VID/PID pair that's assigned to the hardware
#define USB_VENDOR_ID  0x1d50 // Openmoko, Inc
#define USB_PRODUCT_ID 0x6028 // ErgoDox ergonomic keyboard

// But define different descriptive strings
#define USB_MANUFACTURER_STRING L"andreae.gen.nz"
#define USB_PRODUCT_STRING L"Programmable USB Keyboard"
#define USB_SERIAL_NUMBER_STRING L"andreae.gen.nz:ergodox"

// Unique identifier representing this keyboard's layout and
// definition of logical_keycode values.  Is reported to the
// configuration program over USB to identify the layout.
#define LAYOUT_ID 2

/* Storage layout */
#define CONSTANT_STORAGE           avr_pgm
#define MAPPING_STORAGE            avr_eeprom

#ifdef NO_EXTERNAL_STORAGE
	#define SAVED_MAPPING_STORAGE      avr_eeprom
	#define SAVED_MAPPING_COUNT        128          // 2-byte entries
	#define MACRO_INDEX_STORAGE        avr_eeprom
	#define MACRO_INDEX_COUNT          10           // 6-byte entries
	#define MACROS_STORAGE             avr_eeprom
	#define MACROS_SIZE                256
	#define PROGRAM_STORAGE            avr_eeprom
	#define PROGRAM_SIZE               256
	#define PROGRAM_COUNT              2
#else
	#define SAVED_MAPPING_STORAGE      avr_eeprom
	#define SAVED_MAPPING_COUNT        256          // 2-byte entries
	#define MACRO_INDEX_STORAGE        avr_eeprom
	#define MACRO_INDEX_COUNT          50           // 6-byte entries
	#define MACROS_STORAGE             i2c_eeprom
	#define MACROS_SIZE                1024
	#define PROGRAM_STORAGE            i2c_eeprom
	#define PROGRAM_SIZE               1024
	#define PROGRAM_COUNT              6
#endif

#define NUM_PHYSICAL_KEYS 76
#define NUM_LOGICAL_KEYS  NUM_PHYSICAL_KEYS * 2
#define KEYPAD_LAYER_SIZE NUM_PHYSICAL_KEYS

// The Ergodox is made up of two 6x7 matrices, one for each hand. The diodes are
// installed so that we drive what the Ergodox firmware calls the "columns" side
// (7) and scan the "rows" (6). We're going to reverse the naming convention,
// since our convention is that we select "rows" and read "columns".  We select
// the same row on each side (pulling it low) and then scan the columns (input
// with internal pull-ups).

#define MATRIX_COLS 12 // 6 on lhs, 6 on rhs via io expander
#define MATRIX_ROWS 7  // 7 on each side driven synchronously

// Logical keys we have: logical keys represent a key-position+keypad-layer combination.
enum logical_keys {
	// Main key blocks
	LOGICAL_KEY_A,
	LOGICAL_KEY_B,
	LOGICAL_KEY_C,
	LOGICAL_KEY_D,
	LOGICAL_KEY_E,
	LOGICAL_KEY_F,
	LOGICAL_KEY_G,
	LOGICAL_KEY_H,
	LOGICAL_KEY_I,
	LOGICAL_KEY_J,
	LOGICAL_KEY_K,
	LOGICAL_KEY_L,
	LOGICAL_KEY_M,
	LOGICAL_KEY_N,
	LOGICAL_KEY_O,
	LOGICAL_KEY_P,
	LOGICAL_KEY_Q,
	LOGICAL_KEY_R,
	LOGICAL_KEY_S,
	LOGICAL_KEY_T,
	LOGICAL_KEY_U,
	LOGICAL_KEY_V,
	LOGICAL_KEY_W,
	LOGICAL_KEY_X,
	LOGICAL_KEY_Y,
	LOGICAL_KEY_Z,
	LOGICAL_KEY_1,
	LOGICAL_KEY_2,
	LOGICAL_KEY_3,
	LOGICAL_KEY_4,
	LOGICAL_KEY_5,
	LOGICAL_KEY_6,
	LOGICAL_KEY_7,
	LOGICAL_KEY_8,
	LOGICAL_KEY_9,
	LOGICAL_KEY_0,
	LOGICAL_KEY_SEMICOLON,
	LOGICAL_KEY_COMMA,
	LOGICAL_KEY_PERIOD,
	LOGICAL_KEY_SLASH,

	// Left hand extra keys
	LOGICAL_KEY_LCOL1_1, // outer column
	LOGICAL_KEY_LCOL1_2,
	LOGICAL_KEY_LCOL1_3,
	LOGICAL_KEY_LCOL1_4,
	LOGICAL_KEY_LROW1, // bottom row
	LOGICAL_KEY_LROW2,
	LOGICAL_KEY_LROW3,
	LOGICAL_KEY_LROW4,
	LOGICAL_KEY_LROW5,
	LOGICAL_KEY_LCOL2_1, // inner column (top-most keys in this column are pgm/kpd)
	LOGICAL_KEY_LCOL2_2,
	LOGICAL_KEY_LCOL2_3,

	// Right hand extra keys
	LOGICAL_KEY_RCOL1_1, // outer column
	LOGICAL_KEY_RCOL1_2,
	LOGICAL_KEY_RCOL1_3,
	LOGICAL_KEY_RCOL1_4,
	LOGICAL_KEY_RROW1, // bottom row
	LOGICAL_KEY_RROW2,
	LOGICAL_KEY_RROW3,
	LOGICAL_KEY_RROW4,
	LOGICAL_KEY_RROW5,
	LOGICAL_KEY_RCOL2_1, // inner column
	LOGICAL_KEY_RCOL2_2,
	LOGICAL_KEY_RCOL2_3,

	// Left hand thumbpad
	LOGICAL_KEY_L_ALT,
	LOGICAL_KEY_L_CTRL,
	LOGICAL_KEY_HOME,
	LOGICAL_KEY_END,
	LOGICAL_KEY_BACKSPACE,
	LOGICAL_KEY_DELETE,

	// Right hand thumb pad
	LOGICAL_KEY_R_ALT,
	LOGICAL_KEY_R_CTRL,
	LOGICAL_KEY_PGUP,
	LOGICAL_KEY_PGDN,
	LOGICAL_KEY_ENTER,
	LOGICAL_KEY_SPACE
	// The keypad layer duplicates the previous 74 keys
};

// Which logical keys to use for special in-built combinations
#define SPECIAL_HKEY_MACRO_RECORD  HID_KEYBOARD_SC_M
#define SPECIAL_HKEY_REMAP         HID_KEYBOARD_SC_P
#define SPECIAL_HKEY_REBOOT        HID_KEYBOARD_SC_B
#define SPECIAL_HKEY_RESET_CONFIG  HID_KEYBOARD_SC_Z
#define SPECIAL_HKEY_RESET_FULLY   HID_KEYBOARD_SC_LEFT_SHIFT

extern const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM;

/* For each key, maps an index position to a default HID key code. */
/* stored in flash. */
extern const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM;


//////////////////////////////////////////////////////////////////////////////////////
// Ergodox board layout

// Input: Note that we're switching the definitions of 'row' and 'column' from
// the Ergodox internals: we drive rows and scan columns.

// Right hand rows and columns
// 7 rows: PB0-3 + PD2-3 + PC6
// 6 cols: PF0-1 + PF4-7

// Left hand MCP23018 I/O expander connected via I2C (PD0=SCL, PD1=SDA)
// Address is set to 0b0100000
// 7 rows: GPA0-6
// 6 cols: GPB0-5

// Output:
// 3 LEDs on PB5,6,7 - source current to enable
// Onboard LED on PD6

// For now, no buzzer, no eeprom.

////////////////////////////////////////////////////////////////////////////////////////

#define RIGHT_MATRIX_IN_PIN  PINF
#define RIGHT_MATRIX_IN_PORT PORTF
#define RIGHT_MATRIX_IN_DDR  DDRF
#define RIGHT_MATRIX_IN_MASK 0b11110011

#define RIGHT_MATRIX_OUT_1_PORT PORTB
#define RIGHT_MATRIX_OUT_1_DDR  DDRB
#define RIGHT_MATRIX_OUT_1_MASK 0b00001111

#define RIGHT_MATRIX_OUT_2_PORT PORTD
#define RIGHT_MATRIX_OUT_2_DDR  DDRD
#define RIGHT_MATRIX_OUT_2_MASK 0b00001100
#define RIGHT_MATRIX_OUT_2_START (1<<2)

#define RIGHT_MATRIX_OUT_3_PORT PORTC
#define RIGHT_MATRIX_OUT_3_DDR  DDRC
#define RIGHT_MATRIX_OUT_3_MASK (1<<6)

#define USE_BUZZER 0

#define LED_PORT PORTB
#define LED_DDR  DDRB
#define LED_CAPS (1<<5)
#define LED_NUMLOCK (1<<6)
#define LED_KEYPAD (1<<7)
#define ALL_LEDS (LED_CAPS | LED_NUMLOCK | LED_KEYPAD)

// portd6 is OC4D - could drive it with timer4
#define LED_EXTRA_PORT PORTD
#define LED_EXTRA_DDR  DDRD
#define LED_EXTRA (1<<6)

// EEPROM not initially installed.
#define EEPROM_PORT PORTD
#define EEPROM_DDR  DDRD
#define EEPROM_PIN  PIND
#define EEPROM_SCL (1<<0)
#define EEPROM_SDA (1<<1)

#define MCP23018_ADDR  0b01000000
#define MCP23018_WRITE 0
#define MCP23018_READ  1

// The MCP23018 initializes in bank 0 sequential-access mode,
// so registers are as in table 1-3, 1-5.

#define MCP23018_IODIRA 0x00  // ~ DDR
#define MCP23018_IODIRB 0x01
#define MCP23018_GPPUA  0x0C  // pull-up config
#define MCP23018_GPPUB  0x0D
#define MCP23018_GPIOA  0x12  // ~ PIN
#define MCP23018_GPIOB  0x13
#define MCP23018_OLATA  0x14  // ~ PORT
#define MCP23018_OLATB  0x15


void ports_init(void);

/**
 * Gets the current physical input for a given physical position
 */
void matrix_select_row(uint8_t matrix_row);
uint8_t matrix_read_column(uint8_t matrix_column);

/* Macros: */
/** LED mask for the library LED driver, to indicate that the USB interface is not ready. */
#define LEDMASK_USB_NOTREADY     (LED_KEYPAD | LED_NUMLOCK)

/** LED mask for the library LED driver, to indicate that the USB interface is enumerating. */
#define LEDMASK_USB_ENUMERATING  (LED_KEYPAD | LED_CAPS)

/** LED mask for the library LED driver, to indicate that the USB interface is ready. */
#define LEDMASK_USB_READY        0

/** LED mask for the library LED driver, to indicate that an error has occurred in the USB interface. */
#define LEDMASK_USB_ERROR        (LED_KEYPAD | LED_CAPS | LED_NUMLOCK)

#define LEDMASK_CAPS      LED_CAPS
#define LEDMASK_NUMLOCK   LED_NUMLOCK
#define LEDMASK_KEYPAD    LED_KEYPAD

#define LEDMASK_PROGRAMMING_SRC (LED_CAPS)
#define LEDMASK_PROGRAMMING_DST (LED_NUMLOCK)
#define LEDMASK_MACRO_TRIGGER   (LED_CAPS)
#define LEDMASK_MACRO_RECORD    (LED_CAPS|LED_NUMLOCK)
#define LEDMASK_ALL ALL_LEDS
#define LEDMASK_NONE 0

void set_all_leds(uint8_t led_mask);

void test_leds(void);

#endif // __KINESIS_H
