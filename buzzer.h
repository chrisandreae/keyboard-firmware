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

#ifndef __BUZZER_H
#define __BUZZER_H

#include "hardware.h"

// define tones for different purposes
#define BUZZER_DEFAULT_TONE 110 // f = 1/(x * 4e-6) Hz

// to indicate success or failure of an action
#define BUZZER_SUCCESS_TONE 90
#define BUZZER_FAILURE_TONE 220

// for enabling and disabling a setting
#define BUZZER_ON_TONE 75
#define BUZZER_OFF_TONE 150

#if USE_BUZZER

void buzzer_start(uint16_t ms);
void buzzer_start_f(uint16_t ms, uint8_t freq);
void buzzer_update(uint8_t increment);

#else

#define buzzer_start(x)
#define buzzer_start_f(x,y)
#define buzzer_update(x)

#endif

#endif // __BUZZER_H
