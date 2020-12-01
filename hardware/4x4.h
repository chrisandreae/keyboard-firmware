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

#ifndef __KEYPAD_H
#define __KEYPAD_H

#include "keystate.h"

// Use Obdev's free usb id for keyboards discriminated by serial number
#define USB_VENDOR_ID  0x16c0
#define USB_PRODUCT_ID 0x27dc // Should be b! hacky

// But define different descriptive strings
#define USB_MANUFACTURER_STRING L"andreae.gen.nz"
#define USB_PRODUCT_STRING L"Programmable USB 4x4 Keypad"
#define USB_SERIAL_NUMBER_STRING L"andreae.gen.nz:4x4"

// Unique identifier representing this keyboard's layout and
// definition of logical_keycode values.  Is reported to the
// configuration program over USB to identify the layout.
#define LAYOUT_ID 4

/* Storage layout */
#define CONSTANT_STORAGE           avr_pgm
#define MAPPING_STORAGE            avr_eeprom

#define SAVED_MAPPING_STORAGE      avr_eeprom
#define SAVED_MAPPING_COUNT        128          // 2-byte entries
#define MACRO_INDEX_STORAGE        avr_eeprom
#define MACRO_INDEX_COUNT          10           // 6-byte entries
#define MACROS_STORAGE             avr_eeprom
#define MACROS_SIZE                256
#define PROGRAM_STORAGE            avr_eeprom
#define PROGRAM_SIZE               256
#define PROGRAM_COUNT              2

#define NUM_PHYSICAL_KEYS 4 * 4
#define NUM_LOGICAL_KEYS  NUM_PHYSICAL_KEYS * 2
#define KEYPAD_LAYER_SIZE NUM_PHYSICAL_KEYS

// Describes an 4x4 matrix keypad, directly driven.

#define MATRIX_ROWS 4  // Drive side.
#define MATRIX_COLS 4 // Read side.

// Logical keys we have: logical keys represent a key-position+keypad-layer combination.
enum logical_keys {
    LOGICAL_KEY_1,
    LOGICAL_KEY_2,
    LOGICAL_KEY_3,
    LOGICAL_KEY_4,
    LOGICAL_KEY_5,
    LOGICAL_KEY_6,
    LOGICAL_KEY_7,
    LOGICAL_KEY_8,
    LOGICAL_KEY_9,
    LOGICAL_KEY_STAR,
    LOGICAL_KEY_0,
    LOGICAL_KEY_HASH,
    LOGICAL_KEY_A,
    LOGICAL_KEY_B,
    LOGICAL_KEY_C,
    LOGICAL_KEY_D
};

// Which logical keys to use for special in-built combinations
#define SPECIAL_HKEY_MACRO_RECORD  HID_KEYBOARD_SC_F1
#define SPECIAL_HKEY_REMAP         HID_KEYBOARD_SC_F2
#define SPECIAL_HKEY_REBOOT        HID_KEYBOARD_SC_F3
#define SPECIAL_HKEY_RESET_CONFIG  HID_KEYBOARD_SC_F4
#define SPECIAL_HKEY_RESET_FULLY   HID_KEYBOARD_SC_F5

extern const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM;

/* For each key, maps an index position to a default HID key code. */
/* stored in flash. */
extern const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM;


//////////////////////////////////////////////////////////////////////////////////////
// Board layout

// Directly driven using an Arduino Pro Micro.
// Matrix rows (drive):    PD0 (3), PD1 (2), PD2 (0), PD3 (1)
// Matrix columns (sense): PB1 (pin 15), PB2 (16), PB3 (14), PB4 (8)

// Output: built in LED on PD5, sink current to enable

// No buzzer, no eeprom.
//////////////////////////////////////////////////////////////////////////////////////

#define ROWS_PORT PORTD
#define ROWS_DDR DDRD
#define ROWS_OFFSET 0
#define ROWS_MASK (0b1111 << ROWS_OFFSET)

#define COLS_PORT PORTB
#define COLS_DDR DDRB
#define COLS_PIN PINB
#define COLS_OFFSET 1
#define COLS_MASK (0b1111 << COLS_OFFSET)

#define USE_BUZZER 0

#define LED_PORT PORTD
#define LED_DDR  DDRD
#define LED_CAPS (1<<5)
#define ALL_LEDS (LED_CAPS)

void ports_init(void);

/**
 * Gets the current physical input for a given physical position
 */
void matrix_select_row(uint8_t matrix_row);
uint8_t matrix_read_column(uint8_t matrix_column);

/* Macros: */
/** LED mask for the library LED driver, to indicate that the USB interface is not ready. */
#define LEDMASK_USB_NOTREADY     (LED_CAPS)

/** LED mask for the library LED driver, to indicate that the USB interface is enumerating. */
#define LEDMASK_USB_ENUMERATING  (LED_CAPS)

/** LED mask for the library LED driver, to indicate that the USB interface is ready. */
#define LEDMASK_USB_READY        0

/** LED mask for the library LED driver, to indicate that an error has occurred in the USB interface. */
#define LEDMASK_USB_ERROR        (LED_CAPS)

#define LEDMASK_CAPS      LED_CAPS
#define LEDMASK_NUMLOCK   0
#define LEDMASK_KEYPAD    0

#define LEDMASK_PROGRAMMING_SRC (LED_CAPS)
#define LEDMASK_PROGRAMMING_DST (LED_CAPS)
#define LEDMASK_MACRO_TRIGGER   (LED_CAPS)
#define LEDMASK_MACRO_RECORD    (LED_CAPS)
#define LEDMASK_ALL ALL_LEDS
#define LEDMASK_NONE 0

void set_all_leds(uint8_t led_mask);

void test_leds(void);

#endif // __KEYPAD_H
