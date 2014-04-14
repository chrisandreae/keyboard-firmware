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

#include "extrareport.h"

#include "Keyboard.h"

void ExtraKeyboardReport_clear(ExtraKeyboardReport* r){
	r->modifiers = 0;
	memset(r->keys, NO_KEY, EXTRA_REPORT_KEY_COUNT);
}

void ExtraKeyboardReport_add(ExtraKeyboardReport* r, hid_keycode key){
	if(key >= HID_KEYBOARD_SC_LEFT_CONTROL){
		r->modifiers |= 1 << (key - HID_KEYBOARD_SC_LEFT_CONTROL);
	}
	else{
		uint8_t free = NO_KEY;
		for(int i = EXTRA_REPORT_KEY_COUNT - 1; i >= 0; --i){
			hid_keycode p = r->keys[i];
			if(p == key) return; // Key already pressed, nothing to do
			else if(p == NO_KEY){
				free = i;
			}
		}
		if(free != NO_KEY){
			r->keys[free] = key;
		}
	}
}

void ExtraKeyboardReport_remove(ExtraKeyboardReport* r, hid_keycode key){
	if(key >= HID_KEYBOARD_SC_LEFT_CONTROL){
		r->modifiers &= ~(1 << (key - HID_KEYBOARD_SC_LEFT_CONTROL));
	}
	else{
		for(int i = 0; i < EXTRA_REPORT_KEY_COUNT; ++i){
			if(r->keys[i] == key){
				r->keys[i] = NO_KEY;
				return;
			}
		}
	}
}

void ExtraKeyboardReport_toggle(ExtraKeyboardReport* r, hid_keycode key){
	if(key >= HID_KEYBOARD_SC_LEFT_CONTROL){
		r->modifiers ^= 1 << (key - HID_KEYBOARD_SC_LEFT_CONTROL);
	}
	else{
		uint8_t free = NO_KEY;
		for(int i = EXTRA_REPORT_KEY_COUNT - 1; i >= 0; --i){
			hid_keycode p = r->keys[i];
			if(p == key){
				// was on, turn off, done
				r->keys[i] = NO_KEY;
				return;
			}
			else if(r->keys[i] == NO_KEY){
				free = i;
			}
		}
		if(free != NO_KEY){
			r->keys[free] = key;
		}
	}
}

void ExtraKeyboardReport_append(ExtraKeyboardReport* extra, KeyboardReport_Data_t* report){
	// find free slots in the report
	uint8_t report_next, report_start;
	for(report_next = 0; report_next < KEYBOARDREPORT_KEY_COUNT && report->KeyCode[report_next]; ++report_next);
	report_start = report_next;

	// add in modifier keys
	report->Modifier |= extra->modifiers;

	// as long as there are free slots, add in keys from extra report that are not already present.
	for(uint8_t k = 0; report_next < KEYBOARDREPORT_KEY_COUNT && k < EXTRA_REPORT_KEY_COUNT; ++k){
		hid_keycode keycode = extra->keys[k];
		if(keycode == NO_KEY) continue;
		for(uint8_t j = 0; j < report_start; ++j){
			if(report->KeyCode[j] == keycode) goto next_vk; // already pressed: labelled-continue
		}
		report->KeyCode[report_next++] = keycode;
	next_vk:;
	}
}
