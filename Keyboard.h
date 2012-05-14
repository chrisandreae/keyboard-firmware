/*
  LUFA Library
  Copyright (C) Dean Camera, 2011.

  dean [at] fourwalledcubicle [dot] com
  www.lufa-lib.org
*/

/*
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

/** \file
 *
 *  Header file for Keyboard.c.
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

#include <LUFA/Version.h>
#include <LUFA/Drivers/USB/USB.h>

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

#define DEBOUNCE_MASK 0x07 // care about last 3 physical reports

void updateLEDs(void);
void blinkLEDs(void);

void KeyState_Init(void);
void KeyState_Update(void);

bool KeyState_CheckKey(logical_keycode key);
bool KeyState_CheckKeys(uint8_t count, ...);
void KeyState_GetKeys(logical_keycode* keys);

void Eeprom_ResetDefaults(void);
void Eeprom_ResetFully(void);
uint8_t Eeprom_DeleteLayout(uint8_t num);
uint8_t Eeprom_SaveLayout(uint8_t num);
uint8_t Eeprom_LoadLayout(uint8_t num);

void EVENT_USB_Device_Connect(void);
void EVENT_USB_Device_Disconnect(void);
void EVENT_USB_Device_ConfigurationChanged(void);
void EVENT_USB_Device_ControlRequest(void);
void EVENT_USB_Device_StartOfFrame(void);

bool CALLBACK_HID_Device_CreateHIDReport(USB_ClassInfo_HID_Device_t* const HIDInterfaceInfo,
										 uint8_t* const ReportID,
										 const uint8_t ReportType,
										 void* ReportData,
										 uint16_t* const ReportSize);

void CALLBACK_HID_Device_ProcessHIDReport(USB_ClassInfo_HID_Device_t* const HIDInterfaceInfo,
										  const uint8_t ReportID,
										  const uint8_t ReportType,
										  const void* ReportData,
										  const uint16_t ReportSize);

#endif

