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

#ifndef _USB_VENDOR_INTERFACE_H_
#define _USB_VENDOR_INTERFACE_H_

typedef enum _vendor_request {
	READ_LAYOUT_ID,    // Which type of keyboard are we, what do the logical keycodes mean?
	READ_MAPPING_SIZE, // How many logical keycodes do we map?
	WRITE_MAPPING, READ_MAPPING,
	READ_DEFAULT_MAPPING,

	READ_NUM_PROGRAMS,  // How many program VMs do we run?
	READ_PROGRAMS_SIZE, // How much space is available for program storage?
	WRITE_PROGRAMS, READ_PROGRAMS,

	RESET_DEFAULTS,
	RESET_FULLY,

	READ_CONFIG_FLAGS,
	WRITE_CONFIG_FLAGS, // one byte: passed in wvalue

	READ_MACRO_INDEX_SIZE,
	WRITE_MACRO_INDEX, READ_MACRO_INDEX,
	READ_MACRO_STORAGE_SIZE,
	WRITE_MACRO_STORAGE, READ_MACRO_STORAGE,
	READ_MACRO_MAX_KEYS

} vendor_request;

#endif //_USB_VENDOR_INTERFACE_H_
