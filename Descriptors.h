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


#ifndef _DESCRIPTORS_H_
#define _DESCRIPTORS_H_

	/* Includes: */
		#include <avr/pgmspace.h>

#ifdef BUILD_FOR_LUFA
		#include <LUFA/Drivers/USB/USB.h>
#else
		#include <LUFA_compat.h> // Stripped types from LUFA headers
#endif

	/* Type Defines: */
		/** Type define for the device configuration descriptor structure. This must be defined in the
		 *  application code, as the configuration descriptor contains several sub-descriptors which
		 *  vary between devices, and which describe the device's usage to the host.
		 */
		typedef struct
		{
			USB_Descriptor_Configuration_Header_t Config;
			USB_Descriptor_Interface_t            HID1_KeyboardInterface;
			USB_HID_Descriptor_HID_t              HID1_KeyboardHID;
			USB_Descriptor_Endpoint_t             HID1_ReportINEndpoint;
			USB_Descriptor_Interface_t            HID2_MouseInterface;
			USB_HID_Descriptor_HID_t              HID2_MouseHID;
			USB_Descriptor_Endpoint_t             HID2_ReportINEndpoint;
		} USB_Descriptor_Configuration_t;

		typedef struct
		{
			uint8_t Button; /**< Button mask for currently pressed buttons in the mouse. */
			int8_t  X; /**< Current delta X movement of the mouse. */
			int8_t  Y; /**< Current delta Y movement on the mouse. */
			int8_t  Wheel;
		} __attribute__((packed)) MouseReport_Data_t;

		#define KEYBOARDREPORT_KEY_COUNT 6

		typedef struct
		{
			uint8_t Modifier; /**< Keyboard modifier byte, indicating pressed modifier keys (a combination of
							   *   \c HID_KEYBOARD_MODIFER_* masks).
							   */
			uint8_t Reserved; /**< Reserved for OEM use, always set to 0. */
			uint8_t KeyCode[KEYBOARDREPORT_KEY_COUNT]; /**< Key codes of the currently pressed keys. */
		} __attribute__((packed)) KeyboardReport_Data_t;

	/* Macros: */
		/** Endpoint number of the Keyboard HID reporting IN endpoint. */
		#define KEYBOARD_IN_EPNUM               1

		/** Endpoint number of the Mouse HID reporting IN endpoint. */
		#define MOUSE_IN_EPNUM            3

		/** Size in bytes of the Keyboard HID reporting IN and OUT endpoints. */
		#define HID_EPSIZE           8


#endif
