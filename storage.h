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

#ifndef __STORAGE_H
#define __STORAGE_H

#include <inttypes.h>

typedef enum _storage_type {
    sram,
    avr_pgm,
    avr_eeprom,
    i2c_eeprom,
    flash,
} storage_type;

typedef uint8_t storage_err;
extern storage_err storage_errno;

#define STORAGE_MAGIC_PREFIX(x, y) x ## _ ## y

#define STORAGE(storage_type)                                  STORAGE_MAGIC_PREFIX(STORAGE_SECTION, storage_type)

#define storage_write(storage_type, dst, buf, count)           STORAGE_MAGIC_PREFIX(storage_type, write)(dst, buf, count)
#define storage_write_byte(storage_type, dst, b)               STORAGE_MAGIC_PREFIX(storage_type, write_byte)(dst, b)
#define storage_write_short(storage_type, dst, b)              STORAGE_MAGIC_PREFIX(storage_type, write_short)(dst, b)
#define storage_write_step(storage_type, dst, data, len, last) STORAGE_MAGIC_PREFIX(storage_type, write_step)(dst, data, len, last)

#define storage_read(storage_type, addr, buf, len)             STORAGE_MAGIC_PREFIX(storage_type, read)(addr, buf, len)
#define storage_read_byte(storage_type, addr)                  STORAGE_MAGIC_PREFIX(storage_type, read_byte)(addr)
#define storage_read_short(storage_type, addr)                 STORAGE_MAGIC_PREFIX(storage_type, read_short)(addr)

#define storage_memmove(storage_type, dst, src, count)         STORAGE_MAGIC_PREFIX(storage_type, memmove)(dst, src, count)
#define storage_memset(storage_type, dst, c, len)              STORAGE_MAGIC_PREFIX(storage_type, memset)(dst, c, len)

#include "storage/sram.h"
#include "storage/avr_eeprom.h"
#include "storage/i2c_eeprom.h"
#include "storage/avr_pgm.h"

#endif // __STORAGE_H
