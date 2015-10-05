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
#define USB_PRODUCT_STRING L"Programmable USB Keypad"
#define USB_SERIAL_NUMBER_STRING L"andreae.gen.nz:keypad"

// Unique identifier representing this keyboard's layout and
// definition of logical_keycode values.  Is reported to the
// configuration program over USB to identify the layout.
#define LAYOUT_ID 3

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

#define NUM_PHYSICAL_KEYS 8 * 6 * 2
#define NUM_LOGICAL_KEYS  NUM_PHYSICAL_KEYS * 2
#define KEYPAD_LAYER_SIZE NUM_PHYSICAL_KEYS

// The The keyboard is two 8x8 matrixes driven simultaneously via a pair of 164/165 shift registers.

#define MATRIX_ROWS 8  // Drive side. On this keyboard, actually columns.
#define MATRIX_COLS 16 // Read side.  On this keyboard, actually rows.

// Logical keys we have: logical keys represent a key-position+keypad-layer combination.
enum logical_keys {
	LOGICAL_KEY_RHS_TP1,
	LOGICAL_KEY_RHS_TP2,
	LOGICAL_KEY_RHS_TP3,
	LOGICAL_KEY_RHS_TP4,
	LOGICAL_KEY_RHS_TP5,
	LOGICAL_KEY_RHS_TP6,
	LOGICAL_KEY_RHS_C1R1,
	LOGICAL_KEY_RHS_C1R2,
	LOGICAL_KEY_RHS_C1R3,
	LOGICAL_KEY_RHS_C1R4,
	LOGICAL_KEY_RHS_C1R5,
	LOGICAL_KEY_RHS_C1R6,
	LOGICAL_KEY_RHS_C2R1,
	LOGICAL_KEY_RHS_C2R2,
	LOGICAL_KEY_RHS_C2R3,
	LOGICAL_KEY_RHS_C2R4,
	LOGICAL_KEY_RHS_C2R5,
	LOGICAL_KEY_RHS_C2R6,
	LOGICAL_KEY_RHS_C3R1,
	LOGICAL_KEY_RHS_C3R2,
	LOGICAL_KEY_RHS_C3R3,
	LOGICAL_KEY_RHS_C3R4,
	LOGICAL_KEY_RHS_C3R5,
	LOGICAL_KEY_RHS_C3R6,
	LOGICAL_KEY_RHS_C4R1,
	LOGICAL_KEY_RHS_C4R2,
	LOGICAL_KEY_RHS_C4R3,
	LOGICAL_KEY_RHS_C4R4,
	LOGICAL_KEY_RHS_C4R5,
	LOGICAL_KEY_RHS_C4R6,
	LOGICAL_KEY_RHS_C5R1,
	LOGICAL_KEY_RHS_C5R2,
	LOGICAL_KEY_RHS_C5R3,
	LOGICAL_KEY_RHS_C5R4,
	LOGICAL_KEY_RHS_C5R5,
	LOGICAL_KEY_RHS_C5R6,
	LOGICAL_KEY_RHS_C6R1,
	LOGICAL_KEY_RHS_C6R2,
	LOGICAL_KEY_RHS_C6R3,
	LOGICAL_KEY_RHS_C6R4,
	LOGICAL_KEY_RHS_C6R5,
	LOGICAL_KEY_RHS_C6R6,
	LOGICAL_KEY_RHS_C7R1,
	LOGICAL_KEY_RHS_C7R2,
	LOGICAL_KEY_RHS_C7R3,
	LOGICAL_KEY_RHS_C7R4,
	LOGICAL_KEY_RHS_C7R5,
	LOGICAL_KEY_RHS_C7R6,

	LOGICAL_KEY_LHS_TP1,
	LOGICAL_KEY_LHS_TP2,
	LOGICAL_KEY_LHS_TP3,
	LOGICAL_KEY_LHS_TP4,
	LOGICAL_KEY_LHS_TP5,
	LOGICAL_KEY_LHS_TP6,
	LOGICAL_KEY_LHS_C1R1,
	LOGICAL_KEY_LHS_C1R2,
	LOGICAL_KEY_LHS_C1R3,
	LOGICAL_KEY_LHS_C1R4,
	LOGICAL_KEY_LHS_C1R5,
	LOGICAL_KEY_LHS_C1R6,
	LOGICAL_KEY_LHS_C2R1,
	LOGICAL_KEY_LHS_C2R2,
	LOGICAL_KEY_LHS_C2R3,
	LOGICAL_KEY_LHS_C2R4,
	LOGICAL_KEY_LHS_C2R5,
	LOGICAL_KEY_LHS_C2R6,
	LOGICAL_KEY_LHS_C3R1,
	LOGICAL_KEY_LHS_C3R2,
	LOGICAL_KEY_LHS_C3R3,
	LOGICAL_KEY_LHS_C3R4,
	LOGICAL_KEY_LHS_C3R5,
	LOGICAL_KEY_LHS_C3R6,
	LOGICAL_KEY_LHS_C4R1,
	LOGICAL_KEY_LHS_C4R2,
	LOGICAL_KEY_LHS_C4R3,
	LOGICAL_KEY_LHS_C4R4,
	LOGICAL_KEY_LHS_C4R5,
	LOGICAL_KEY_LHS_C4R6,
	LOGICAL_KEY_LHS_C5R1,
	LOGICAL_KEY_LHS_C5R2,
	LOGICAL_KEY_LHS_C5R3,
	LOGICAL_KEY_LHS_C5R4,
	LOGICAL_KEY_LHS_C5R5,
	LOGICAL_KEY_LHS_C5R6,
	LOGICAL_KEY_LHS_C6R1,
	LOGICAL_KEY_LHS_C6R2,
	LOGICAL_KEY_LHS_C6R3,
	LOGICAL_KEY_LHS_C6R4,
	LOGICAL_KEY_LHS_C6R5,
	LOGICAL_KEY_LHS_C6R6,
	LOGICAL_KEY_LHS_C7R1,
	LOGICAL_KEY_LHS_C7R2,
	LOGICAL_KEY_LHS_C7R3,
	LOGICAL_KEY_LHS_C7R4,
	LOGICAL_KEY_LHS_C7R5,
	LOGICAL_KEY_LHS_C7R6,
	// The keypad layer duplicates the previous 16 keys
};

// Which logical keys to use for special in-built combinations
#define SPECIAL_HKEY_MACRO_RECORD  HID_KEYBOARD_SC_F7
#define SPECIAL_HKEY_REMAP         HID_KEYBOARD_SC_F8
#define SPECIAL_HKEY_REBOOT        HID_KEYBOARD_SC_F5
#define SPECIAL_HKEY_RESET_CONFIG  HID_KEYBOARD_SC_F4
#define SPECIAL_HKEY_RESET_FULLY   HID_KEYBOARD_SC_LEFT_SHIFT

extern const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM;

/* For each key, maps an index position to a default HID key code. */
/* stored in flash. */
extern const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM;


//////////////////////////////////////////////////////////////////////////////////////
// Board layout

// Using an Arduino Pro Micro and a pair of serial-to-parallel (164) and
// parallel-to-serial (165) shift registers.

// Pin 9 (PB5) - P2S parallel load (!PL: pin 1), S2P serial input (DSA: pin 1) (W6)
// Pin 8 (PB4) - P2S clock (CP: pin 2)                                         (W7)
// Pin 7 (PE6) - RHS P2S output (Q7: pin 9)                                    (W3)
// Pin 6 (PD7) - S2P clock (CP: pin 8)                                         (W9)
// Pin 5 (PC6) - LHS P2S output (Q7: pin 9)                                    (W8)

// Output: built in LED on PD5, sink current to enable

// No buzzer, no eeprom.
//////////////////////////////////////////////////////////////////////////////////////

#define P2S_LOAD_S2P_SER_PORT PORTB
#define P2S_LOAD_S2P_SER_DDR  DDRB
#define P2S_LOAD_S2P_SER_MASK (1<<5)

#define P2S_CLOCK_PORT PORTB
#define P2S_CLOCK_DDR  DDRB
#define P2S_CLOCK_MASK (1<<4)

#define RHS_P2S_DATA_PIN  PINE
#define RHS_P2S_DATA_PORT PORTE
#define RHS_P2S_DATA_DDR  DDRE
#define RHS_P2S_DATA_MASK (1<<6)

#define LHS_P2S_DATA_PIN  PINC
#define LHS_P2S_DATA_PORT PORTC
#define LHS_P2S_DATA_DDR  DDRC
#define LHS_P2S_DATA_MASK (1<<6)

#define S2P_CLOCK_PORT PORTD
#define S2P_CLOCK_DDR  DDRD
#define S2P_CLOCK_MASK (1<<7)

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
