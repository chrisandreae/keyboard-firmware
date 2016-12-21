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

#ifndef __I2C_EEPROM_H
#define __I2C_EEPROM_H

#include "hardware.h"

#include "twi.h"

#define EEEXT __attribute__((section(".eeexternal")))

#define STORAGE_SECTION_i2c_eeprom EEEXT

#define EEEXT_PAGE_SIZE 16

typedef enum _i2c_eeprom_err {
	SUCCESS = 0,
	WSELECT_ERROR,
	RSELECT_ERROR,
	ADDRESS_ERROR,
	DATA_ERROR
} i2c_eeprom_err;

/**
 * Writes count bytes to serial eeprom address dst, potentially using
 * multiple page writes. Returns number of bytes written if any bytes
 * were successfully written, otherwise -1. A return value of less
 * than count indicates that an error occurred and i2c_eeprom_errno
 * is set to indicate the error.
 */
int16_t i2c_eeprom_write(void* dst, const void* data, size_t count);

/**
 * Writes the argument byte to serial eeprom address dst
 */
i2c_eeprom_err i2c_eeprom_write_byte(uint8_t* dst, uint8_t b);

/**
 * Writes the argument short to serial eeprom address dst
 */
i2c_eeprom_err i2c_eeprom_write_short(uint8_t* dst, uint16_t b);

/**
 * Repeatedly called to incrementally write chunks of data to eeprom.
 * The caller is responsible for ensuring that the range to be written
 * does not cross a page boundary, and that writing begins at a page
 * boundary.  Function automatically starts a page write if addr is at
 * a page boundary, and stops the page write after writing if addr+len
 * is a page boundary, or if 'last' is set.
 *
 * Returns i2c_eeprom_err.
 */
i2c_eeprom_err i2c_eeprom_write_step(void* dst, const void* data, uint8_t len, uint8_t last);

size_t i2c_eeprom_read(const void* addr, void* buf, size_t len);

uint8_t i2c_eeprom_read_byte(const uint8_t* addr);

uint16_t i2c_eeprom_read_short(const uint16_t* addr);

i2c_eeprom_err i2c_eeprom_memmove(void* dst, const void* src, size_t count);

i2c_eeprom_err i2c_eeprom_memset(void* dst, uint8_t c, size_t len);

#ifdef DEBUG
// test code (not normally linked)
uint8_t i2c_eeprom_test_read(void);
uint8_t i2c_eeprom_test_write(void);
#endif

#endif // __I2C_EEPROM_H
