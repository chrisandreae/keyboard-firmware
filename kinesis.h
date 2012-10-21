/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  This file is offered under either of the GNU GPL v2 or MIT licences
  below in order that it may be used with either of the V-USB or LUFA
  USB libraries.

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

#ifndef __KINESIS_H
#define __KINESIS_H

#include "keystate.h"

// Unique identifier representing this keyboard's layout and
// definition of logical_keycode values.  Is reported to the
// configuration program over USB to identify the layout.
#define LAYOUT_ID 1

/* Kinesis Matrix */

// The Kinesis has few physical keys, so has a "keypad layer" toggle
// which has a separate binding for each key. This provides access not
// only to the numpad, but to the windows and menu keys.
#define KEYPAD_LAYER 1

// if the keypad mode is selected, and a logical key greater than KEYPAD_LAYER_START
// is read, add KEYPAD_LAYER_SIZE to look up the mapping
#define KEYPAD_LAYER_START 2
#define KEYPAD_LAYER_SIZE  84

#define NUM_LOGICAL_KEYS (2 + (KEYPAD_LAYER_SIZE * 2)) //86 physical keys, 84 of which (all but keypad/program) have a separate keypad layer mapping

#define MATRIX_COLS 10 // 8 demultiplexer selected matrix columns, and two direct button inputs (pins 5 and 6)
#define MATRIX_ROWS 16 // 2 74LS138 1-of-8 demultiplexers

// Logical keys we have
enum logical_keys {
	LOGICAL_KEY_KEYPAD, // KY
	LOGICAL_KEY_PROGRAM, // PG
	// non-keypad layer
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
	LOGICAL_KEY_F1,
	LOGICAL_KEY_F2,
	LOGICAL_KEY_F3,
	LOGICAL_KEY_F4,
	LOGICAL_KEY_F5,
	LOGICAL_KEY_F6,
	LOGICAL_KEY_F7,
	LOGICAL_KEY_F8,
	LOGICAL_KEY_F9,
	LOGICAL_KEY_F10,
	LOGICAL_KEY_F11,
	LOGICAL_KEY_F12,
	LOGICAL_KEY_EQUALS,
	LOGICAL_KEY_TAB,
	LOGICAL_KEY_ESC,
	LOGICAL_KEY_CAPSLOCK,
	LOGICAL_KEY_LSHIFT,
	LOGICAL_KEY_UP,
	LOGICAL_KEY_DOWN,
	LOGICAL_KEY_LEFT,
	LOGICAL_KEY_RIGHT,
	LOGICAL_KEY_LSQUARE,
	LOGICAL_KEY_RSQUARE,
	LOGICAL_KEY_SCROLLLOCK,
	LOGICAL_KEY_PRINTSCREEN,
	LOGICAL_KEY_INTERNATIONAL, // insert in keypad layer
	LOGICAL_KEY_BACKTICK,
	LOGICAL_KEY_HYPHEN,
	LOGICAL_KEY_PAUSE,
	LOGICAL_KEY_END,
	LOGICAL_KEY_HOME,
	LOGICAL_KEY_DELETE,
	LOGICAL_KEY_BACKSPACE,
	LOGICAL_KEY_L_ALT,
	LOGICAL_KEY_L_CTRL,
	LOGICAL_KEY_BACKSLASH,
	LOGICAL_KEY_PGDN,
	LOGICAL_KEY_PGUP,
	LOGICAL_KEY_ENTER,
	LOGICAL_KEY_SPACE,
	LOGICAL_KEY_R_ALT,
	LOGICAL_KEY_R_CTRL,
	LOGICAL_KEY_SEMICOLON,
	LOGICAL_KEY_QUOTE,
	LOGICAL_KEY_COMMA,
	LOGICAL_KEY_PERIOD,
	LOGICAL_KEY_SLASH,
	LOGICAL_KEY_RSHIFT,
	// The keypad layer duplicates the previous 84 keys
};

extern const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM;

/* For each key, maps an index position to a default HID key code. */
/* stored in flash. */
extern const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM;


//////////////////////////////////////////////////////////////////////////////////////
// Kinesis board and uC Port Layout (for AT90USB162)

// 74LS138 demultiplexer matrix select output:
//  A-C connect to the address pins 1-3 of the LS138s, externally pulled up.
//  P138SEL connects to pin 4 (negated input) of the first and pin 6 of the second,
//  to select between them, also externally pulled up.
//  The opposing pins are tied to VCC and ground respectively.
//  Pin 10 ("WP") is connected to pin 5 of both demultiplexers, and must be pulled low to enable them.
//    A (pin 39) = C2
//    B (pin 38) = C4
//    C (pin 37) = C5
//    P138SEL (pin 36) = C6

// Key input:
//  Pressing the key pulls low. I don't think there's an external pull-up, so am using internal pull-ups.
//    keypad (pin 5) - D4,
//    program (pin 6) - C7,
//    matrix output (pins 21-28) - all of port B
//    foot switch 1 (pin 11) - NC
//    foot switch 2 (pin 17) - NC (note that these are likely to differ on the 3-foot-switch Advantage models)

// output:
//    4 LEDs (pins 1-4, sink current to enable) - D0-D3 (num=1, scrolllock=2, kpd=3, caps=4)
//    buzzer (pin 32) = NC

// EEPROM access:
//  have access to up to two AT24C164 2kb serial eeproms via pins 7 and
//  8, connected to SCL and SDA respectively with external pull-ups.
//  The eeproms have their addresse lines tied to 000 and 001 respectively.
//         WP  (write protect) (pin10) = connected to WP pin of eeprom, must be low to enable.
//                                       (note that this is also connected to the demultiplexers)
//         SCL (serial eeprom clock line) (pin7) = NC
//         SDA (serial eeprom data line) (pin8) =  NC

// unavailable-ish ports on uC:
//   d5 and d6 overlap onboard LEDB and A on my AT90USB162 dev board,
//   which means they're pulled up extremely strongly. I don't know
//   whether I can sink enough current to make a device I'm talking to
//   detect a 0.

// static configuration:
//   pin 20 - GND
//   pin 40 - VCC

// unused pins:
//   pin 12 - PS/2 clock
//   pin 13 - PS/2 data
////////////////////////////////////////////////////////////////////////////////////////

#if defined(__AVR_AT90USB162__)

#define MATRIX_PORT PORTC
#define MATRIX_DDR  DDRC
#define MATRIX_SELECT_A (1<<2)
#define MATRIX_SELECT_B (1<<4)
#define MATRIX_SELECT_C (1<<5)
#define MATRIX_SELECT_P138SEL (1<<6)
#define MATRIX_MASK (MATRIX_SELECT_A | MATRIX_SELECT_B | MATRIX_SELECT_C | MATRIX_SELECT_P138SEL)

#define INPUT_PIN5_PIN  PIND
#define INPUT_PIN5_PORT PORTD
#define INPUT_PIN5_DDR  DDRD

#define INPUT_PIN6_PIN  PINC
#define INPUT_PIN6_PORT PORTC
#define INPUT_PIN6_DDR  DDRC

#define INPUT_REST_PIN  PINB
#define INPUT_REST_PORT PORTB
#define INPUT_REST_DDR  DDRB

#define INPUT_PIN5 (1<<4)
#define INPUT_PIN6 (1<<7)
#define INPUT_REST (0xFF)

#define LED_PORT PORTD
#define LED_DDR  DDRD
#define LED_CAPS      (1)
#define LED_NUMLOCK   (1<<1)
#define LED_SCROLLLOCK (1<<2)
#define LED_KEYPAD    (1<<3)
#define INT_LED1 (1<<5)
#define INT_LED2 (1<<6)
#define ALL_LEDS (LED_CAPS | LED_NUMLOCK | LED_SCROLLLOCK | LED_KEYPAD | INT_LED1 | INT_LED2)

#define USE_BUZZER 0
#define USE_EEPROM 0

/*
#define BUZZER_PORT PORTD
#define BUZZER_DDR DDRD
#define BUZZER (1<<4)

#define EEPROM_PORT PORTD
#define EEPROM_DDR  DDRD
#define EEPROM_SCL (1<<6)
#define EEPROM_SDA (1<<7)
*/

#elif defined(__AVR_ATmega16A__)
// Original board layout (v1)

// PD4 = USB D-
// PD2 = USB D+

#define MATRIX_PORT PORTB
#define MATRIX_DDR  DDRB
#define MATRIX_SELECT_A (1<<0)
#define MATRIX_SELECT_B (1<<1)
#define MATRIX_SELECT_C (1<<2)
#define MATRIX_SELECT_P138SEL (1<<3)
#define MATRIX_MASK (MATRIX_SELECT_A | MATRIX_SELECT_B | MATRIX_SELECT_C | MATRIX_SELECT_P138SEL)

// keypad key
#define INPUT_PIN5_PIN  PIND
#define INPUT_PIN5_PORT PORTD
#define INPUT_PIN5_DDR  DDRD
#define INPUT_PIN5 (1<<3)

// program key
#define INPUT_PIN6_PIN  PINC
#define INPUT_PIN6_PORT PORTC
#define INPUT_PIN6_DDR  DDRC
#define INPUT_PIN6 (1<<0)

// FS1 and FS2 are PC5 and PC6

#define INPUT_REST_PIN  PINA
#define INPUT_REST_PORT PORTA
#define INPUT_REST_DDR  DDRA
#define INPUT_REST (0xFF)

#define LED_PORT PORTD
#define LED_DDR  DDRD
#define LED_CAPS (1<<4)
#define LED_NUMLOCK (1<<7)
#define LED_SCROLLLOCK (1<<6)
#define LED_KEYPAD (1<<5)
#define ALL_LEDS (LED_CAPS | LED_NUMLOCK | LED_SCROLLLOCK | LED_KEYPAD)

#define USE_BUZZER 0 // new buzzer code uses OC2 pin, not compatible with this board layout

#define BUZZER_PORT PORTC
#define BUZZER_DDR DDRC
#define BUZZER (1<<3)

#define USE_EEPROM 1
#define BITBANG_TWI // original board bitbangs TWI, doesn't use hardware I2C pins

#define EEPROM_PORT PORTC
#define EEPROM_DDR  DDRC
#define EEPROM_PIN  PINC
#define EEPROM_SCL (1<<1)
#define EEPROM_SDA (1<<2)

#elif defined(__AVR_ATmega32__)
// New board layout (v2)

// PD4 = USB D-
// PD2 = USB D+

#define MATRIX_PORT PORTB
#define MATRIX_DDR  DDRB
#define MATRIX_SELECT_A (1<<0)
#define MATRIX_SELECT_B (1<<1)
#define MATRIX_SELECT_C (1<<2)
#define MATRIX_SELECT_P138SEL (1<<3)
#define MATRIX_MASK (MATRIX_SELECT_A | MATRIX_SELECT_B | MATRIX_SELECT_C | MATRIX_SELECT_P138SEL)

// keypad key
#define INPUT_PIN5_PIN  PINC
#define INPUT_PIN5_PORT PORTC
#define INPUT_PIN5_DDR  DDRC
#define INPUT_PIN5 (1<<3)

// program key
#define INPUT_PIN6_PIN  PINC
#define INPUT_PIN6_PORT PORTC
#define INPUT_PIN6_DDR  DDRC
#define INPUT_PIN6 (1<<2)

// FS1 and FS2 are PC5 and PC6

#define INPUT_REST_PIN  PINA
#define INPUT_REST_PORT PORTA
#define INPUT_REST_DDR  DDRA
#define INPUT_REST (0xFF)

#define LED_PORT PORTD
#define LED_DDR  DDRD
#define LED_CAPS (1<<3)
#define LED_NUMLOCK (1<<6)
#define LED_SCROLLLOCK (1<<5)
#define LED_KEYPAD (1<<4)
#define ALL_LEDS (LED_CAPS | LED_NUMLOCK | LED_SCROLLLOCK | LED_KEYPAD)

#define USE_BUZZER 1
#define USE_EEPROM 1

#define BUZZER_PORT PORTD
#define BUZZER_DDR DDRD
#define BUZZER (1<<7)

#define EEPROM_PORT PORTC
#define EEPROM_DDR  DDRC
#define EEPROM_PIN  PINC
#define EEPROM_SCL (1<<0)
#define EEPROM_SDA (1<<1)

#else
#error Ports not yet defined for this microcontroller
#endif

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
#define LEDMASK_SCROLLLOCK LED_SCROLLLOCK
#define LEDMASK_KEYPAD    LED_KEYPAD

#define LEDMASK_PROGRAMMING_SRC (LED_CAPS|LED_SCROLLLOCK)
#define LEDMASK_PROGRAMMING_DST (LED_NUMLOCK|LED_SCROLLLOCK)
#define LEDMASK_MACRO_TRIGGER   (LED_CAPS|LED_NUMLOCK)
#define LEDMASK_MACRO_RECORD    (LED_CAPS|LED_NUMLOCK|LED_SCROLLLOCK)
#define LEDMASK_ALL ALL_LEDS
#define LEDMASK_NONE 0

void set_all_leds(uint8_t led_mask);

void test_leds(void);

#endif // __KINESIS_H
