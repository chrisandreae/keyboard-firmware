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
#include "4x4.h"

const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM = {
    { LOGICAL_KEY_1, LOGICAL_KEY_2, LOGICAL_KEY_3, LOGICAL_KEY_A },
    { LOGICAL_KEY_4, LOGICAL_KEY_5, LOGICAL_KEY_6, LOGICAL_KEY_B },
    { LOGICAL_KEY_7, LOGICAL_KEY_8, LOGICAL_KEY_9, LOGICAL_KEY_C },
    { LOGICAL_KEY_STAR, LOGICAL_KEY_0, LOGICAL_KEY_HASH, LOGICAL_KEY_D },
};

const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM = {
    HID_KEYBOARD_SC_F1,
    HID_KEYBOARD_SC_F2,
    HID_KEYBOARD_SC_F3,
    HID_KEYBOARD_SC_F4,
    HID_KEYBOARD_SC_F5,
    HID_KEYBOARD_SC_F6,
    HID_KEYBOARD_SC_F7,
    HID_KEYBOARD_SC_F8,
    HID_KEYBOARD_SC_F9,
    SPECIAL_HID_KEY_KEYPAD_TOGGLE, // *
    HID_KEYBOARD_SC_F10,
    HID_KEYBOARD_SC_KEYPAD_HASHMARK, // #
    HID_KEYBOARD_SC_F11,
    HID_KEYBOARD_SC_F12,
    HID_KEYBOARD_SC_F13,
    HID_KEYBOARD_SC_F14,

    // Keypad Layer
    HID_KEYBOARD_SC_KEYPAD_1_AND_END,
    HID_KEYBOARD_SC_KEYPAD_2_AND_DOWN_ARROW,
    HID_KEYBOARD_SC_KEYPAD_3_AND_PAGE_DOWN,
    HID_KEYBOARD_SC_KEYPAD_4_AND_LEFT_ARROW,
    HID_KEYBOARD_SC_KEYPAD_5,
    HID_KEYBOARD_SC_KEYPAD_6_AND_RIGHT_ARROW,
    HID_KEYBOARD_SC_KEYPAD_7_AND_HOME,
    HID_KEYBOARD_SC_KEYPAD_8_AND_UP_ARROW,
    HID_KEYBOARD_SC_KEYPAD_9_AND_PAGE_UP,
    SPECIAL_HID_KEY_KEYPAD_TOGGLE,
    HID_KEYBOARD_SC_KEYPAD_0_AND_INSERT,
    HID_KEYBOARD_SC_KEYPAD_ENTER,
    HID_KEYBOARD_SC_KEYPAD_SLASH,
    HID_KEYBOARD_SC_KEYPAD_ASTERISK,
    HID_KEYBOARD_SC_KEYPAD_MINUS,
    HID_KEYBOARD_SC_KEYPAD_PLUS
};

void ports_init(void) {
    // Row output
    ROWS_DDR  |= ROWS_MASK; // 1 = output
    ROWS_PORT |= ROWS_MASK; // drive high by default

    // Column input
    COLS_DDR  &= ~COLS_MASK; // 0 = input
    COLS_PORT |= COLS_MASK;  // pull-up enabled

    // LEDs sink current: output-low (1,0) to enable, input-highz (0,0) to disable
    LED_DDR  &= ~ALL_LEDS; // start as 0,0
    LED_PORT &= ~ALL_LEDS;
}

void matrix_select_row(uint8_t matrix_row) {
    // pull down the target row
    ROWS_PORT = (ROWS_PORT & ~ROWS_MASK) | (ROWS_MASK & ~(1 << matrix_row << ROWS_OFFSET));
}

uint8_t matrix_read_column(uint8_t matrix_column) {
    return 0 == (COLS_PIN & (1 << matrix_column << COLS_OFFSET)); // button pressed when low
}

void set_all_leds(uint8_t led_mask) {
    led_mask &= ALL_LEDS; // only touch within led range

    LED_DDR = (LED_DDR & ~ALL_LEDS) | led_mask;
}
