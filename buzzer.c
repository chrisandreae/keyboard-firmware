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

#include "buzzer.h"
#include "hardware.h"

#if USE_BUZZER
static uint16_t buzzer_ms;

// cs00 | cs01 : clk/64
// cs22 | cs20 : clk/64

// CLK/64: 4us per tick, enable CTC mode (WGM21)
// buzzer is connected to OC2 pin: toggle OC2 on compare match ((COM20)
static const int TIMER_MODE = ((1<<WGM21) | (1<<COM20) | (1<<CS22) | (1<<CS20));

void buzzer_start(uint16_t ms){
	buzzer_start_f(ms, BUZZER_DEFAULT_FREQ);
}

void buzzer_start_f(uint16_t ms, uint8_t freq){
	if(!buzzer_ms){
		// timer stopped, so turn on the buzzer and start the timer
		BUZZER_PORT |= BUZZER;
		TCNT2  = 0;
		TCCR2 |= TIMER_MODE;
	}
	// always update the frequency
	OCR2 = freq;

	// and update the time remaining (allow it to be cut short)
	buzzer_ms = ms;
}

void buzzer_update(uint8_t increment){
	if(buzzer_ms){
		buzzer_ms = (increment >= buzzer_ms) ? 0 : buzzer_ms - increment;
		if(buzzer_ms == 0){
			// Stop the timer and turn off the buzzer
			TCCR2       &= ~TIMER_MODE;
			BUZZER_PORT &= ~BUZZER;
			TCNT2       =  0;
		}
	}
}


#endif
