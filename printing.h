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

#ifndef __PRINTING_H
#define __PRINTING_H

#include "Keyboard.h"
#include "keystate.h"

#define PGM_MSG(x) ({ static const char __pgm_msg[] PROGMEM = x; __pgm_msg; })

typedef enum __attribute__((__packed__)) _buf_type {
	BUF_MEM, BUF_PGM, BUF_EE, BUF_EEEXT
} buf_type;

void printing_set_buffer(const char* buf, buf_type typ);
uint8_t printing_buffer_empty(void);

void printing_Fill_KeyboardReport(KeyboardReport_Data_t* ReportData);

void char_to_keys(const char nextchar, hid_keycode* nextkey, hid_keycode* nextmod);
const char* byte_to_str(uint8_t byte);

#endif // __PRINTING_H
