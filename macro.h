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

#ifndef __MACRO_H
#define __MACRO_H

#include <stdint.h>

#include <extrareport.h>

#include "macro_index.h"

#ifndef NO_EXTERNAL_STORAGE
	/**
	 * Size in bytes of macro storage (including end offset)
	 */
	#define MACROS_SIZE 1024
#else
	#define MACROS_SIZE 0
#endif

typedef struct _macro_data {
	uint16_t length;
	hid_keycode events[1]; // When encountering a key event, if not pressed, press, else release.
} macro_data;

/**
 * Get the underlying macro data in EEEXT. (To be read/written as a
 * whole by the client application)
 */
uint8_t* macros_get_storage(void);

/**
 * Resets the macro storage - to be called from config_reset_fully
 */
void macros_reset_defaults(void);

/**
 * Starts recording a macro identified by the given key. Adds it to
 * the index, removes any existing data, and returns a pointer to the
 * macro data. Only one macro may be being recorded at once.
 */
bool macros_start_macro(macro_idx_key* key);

/**
 * Commits the currently recording macro which was started with
 * macros_start_macro().  If no events have been appended, instead
 * rolls back the macro creation and removes the entry from the index.
 * This is also used to delete a macro.
 */
void macros_commit_macro(void);

/**
 * Aborts the currently recording macro which was started with
 * macros_start_macro()
 */
void macros_abort_macro(void);

/**
 * Appends the argument HID keycode to the macro being recorded.
 * Returns false if no space left or write failed.
 */
bool macros_append(hid_keycode event);

/**
 * Sets up to play from the macro specified by macro_offset, returns true if successful
 */
bool macros_start_playback(uint16_t macro_offset);

/**
 * Plays the next character, returns true there's more to replay, false if finished.
 */
bool macros_fill_next_report(KeyboardReport_Data_t* report);

#endif // __MACRO_H
