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

#ifndef __SERIAL_EEPROM_H
#define __SERIAL_EEPROM_H

#include "hardware.h"

#if USE_EEPROM
#include "twi.h"

#define EEEXT __attribute__((section(".eeexternal")))

#define EEEXT_PAGE_SIZE 16

typedef enum _serial_eeprom_err {
	SUCCESS = 0,
	WSELECT_ERROR,
	RSELECT_ERROR,
	ADDRESS_ERROR,
	DATA_ERROR
} serial_eeprom_err;

extern serial_eeprom_err serial_eeprom_errno;

serial_eeprom_err serial_eeprom_start_write(uint8_t* addr);
int8_t serial_eeprom_continue_write(const uint8_t* buf, uint8_t len);
static inline void serial_eeprom_end_write(){ twi_stop(); }

int8_t serial_eeprom_write_page(uint8_t* addr, const uint8_t* buf, uint8_t len);

serial_eeprom_err serial_eeprom_write_step(uint8_t* addr, uint8_t* data, uint8_t len, uint8_t last);

int16_t serial_eeprom_read(const uint8_t* addr, uint8_t* buf, uint16_t len);

// test code (not normally linked)
uint8_t serial_eeprom_test_read(void);
uint8_t serial_eeprom_test_write(void);


#endif

#endif // __SERIAL_EEPROM_H
