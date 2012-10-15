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

#ifndef _KEYBOARD_H_
#define _KEYBOARD_H_

/* Includes: */
#include <avr/io.h>
#include <avr/wdt.h>
#include <avr/power.h>
#include <avr/interrupt.h>
#include <stdbool.h>
#include <string.h>

#include "Descriptors.h"

extern volatile uint32_t _uptimems;
static inline uint32_t uptimems(void){ return _uptimems; }

/* Function Prototypes: */
void SetupHardware(void);

typedef enum _state {
	STATE_NORMAL,    // normal keyboard action
	STATE_WAITING,   // Waiting for no keys to be pressed
	STATE_PRINTING,  // outputting the contents of print_buffer
	STATE_PROGRAMMING_SRC, // first key
	STATE_PROGRAMMING_DST, // second key
	STATE_MACRO_RECORD,
	STATE_MACRO_PLAY,
} state;


/** Must be provided by USB driver */
extern void Perform_USB_Update(int update_kbd, int update_mouse);

/** Interface provided to USB driver */
typedef enum _USB_State{ NOTREADY, ENUMERATING, READY, ERROR } USB_State;

void __attribute__((noreturn)) Keyboard_Main(void);
void Update_USBState(USB_State state);
void Update_Millis(uint8_t increment);
void Fill_MouseReport(MouseReport_Data_t* MouseReport);
void Fill_KeyboardReport(KeyboardReport_Data_t* report);
void Process_KeyboardLEDReport(uint8_t report);

/** Buffer to hold the previously generated Keyboard/Mouse HID reports, for comparison purposes inside the HID class driver. */
extern KeyboardReport_Data_t PrevKeyboardHIDReportBuffer;
extern MouseReport_Data_t PrevMouseHIDReportBuffer;

#endif
