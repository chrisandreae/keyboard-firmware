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

#ifndef __KEYSTATE_H
#define __KEYSTATE_H

#include "Keyboard.h"

// types

// Keycodes go through three transformations.
// A raw keyboard matrix code maps to a index in the (keyboard specific) logical key positions table.
// We then look up a HID keycode for this position using the key_defaults/key_config tables.
typedef uint8_t matrix_keycode;
typedef uint8_t logical_keycode;
typedef uint8_t hid_keycode;

typedef struct _key_state {
	logical_keycode l_key;
	unsigned char state:1;
	unsigned char debounce:7; // bit vector of last n physical reports: debounced state asserted when they're all the same
} key_state;

// constants

#define DEBOUNCE_MASK 0x07 // care about last 3 physical reports

#define NO_KEY 0xFF

/* Logical keys are mapped to HID codes. We want to be able to assign some extra actions
   that don't correspond to valid HID codes, so we assign some extra codes for our use
   at the end of the HID range, after E7, the last HID key. As these aren't valid keycodes,
   they'll never be sent via USB
*/
#define SPECIAL_HID_KEYS_START 0xE8
// some special keys are so special we want to forbid ever remapping them.
// We put these after PROGRAM
#define SPECIAL_HID_KEYS_NOREMAP_START SPECIAL_HID_KEY_PROGRAM

#define SPECIAL_HID_KEYS_MOUSE_START SPECIAL_HID_KEY_MOUSE_BTN1
#define SPECIAL_HID_KEYS_MOUSE_END SPECIAL_HID_KEY_MOUSE_RIGHT

enum SPECIAL_HID_KEYS{
	SPECIAL_HID_KEY_MOUSE_BTN1 = 0xE8,
	SPECIAL_HID_KEY_MOUSE_BTN2,
	SPECIAL_HID_KEY_MOUSE_BTN3,
	SPECIAL_HID_KEY_MOUSE_BTN4,
	SPECIAL_HID_KEY_MOUSE_BTN5,
	SPECIAL_HID_KEY_MOUSE_FWD,
	SPECIAL_HID_KEY_MOUSE_BACK,
	SPECIAL_HID_KEY_MOUSE_LEFT,
	SPECIAL_HID_KEY_MOUSE_RIGHT,
	// We'll want placeholder special keys for "look up a macro or
	// program to execute" - if a lkey maps to them, look up that lkey
	// in the macros/programs table
	SPECIAL_HID_KEY_EXEC_PROGRAM1, // 0xF1
	SPECIAL_HID_KEY_EXEC_PROGRAM2,
	SPECIAL_HID_KEY_EXEC_PROGRAM3,
	SPECIAL_HID_KEY_EXEC_PROGRAM4,
	SPECIAL_HID_KEY_EXEC_PROGRAM5,
	SPECIAL_HID_KEY_EXEC_PROGRAM6, // Must have NUM_PROGRAMS keys
	SPECIAL_HID_KEY_EXEC_MACRO,
	// And the extra-special non-remappable program and keypad keys
	SPECIAL_HID_KEY_PROGRAM = 0xFD,
	SPECIAL_HID_KEY_KEYPAD,
};


// fields

extern uint8_t key_press_count;

// Only defined when KEYPAD_LAYER is enabled
extern uint8_t keypad_mode;

void keystate_init(void);

void keystate_update(void);

// Only defined when KEYPAD_LAYER is enabled
void keystate_toggle_keypad(void);

bool keystate_check_key(logical_keycode l_key);

/** returns true if all argument keys are down */
bool keystate_check_keys(uint8_t count, ...);

/** writes up to key_press_count currently pressed key indexes to the
 * output buffer keys */
void keystate_get_keys(logical_keycode* l_keys);

void keystate_Fill_KeyboardReport(KeyboardReport_Data_t* KeyboardReport);

bool keystate_Fill_MouseReport(MouseReport_Data_t* MouseReport);

/** Checks whether a key mapped to the argument keycode (or any key if
 * argument 0) is pressed, returns keycode of first pressed key or
 * NO_KEY.
*/
hid_keycode keystate_check_hid_key(hid_keycode key);

/**
 * Check for keys bound to programs, if found call vm_start
 */
void keystate_run_programs(void);

#endif // __KEYSTATE_H
