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

#include "Keyboard.h"
#include "twi.h"
#include "printing.h"
#include "hardware.h"
#include "storage.h"
#include "storage/i2c_eeprom.h"
#include "usb.h"

#include <util/delay.h>     /* for _delay_ms() */

/* Serial eeprom support */

#define I2C_EEPROM_WRITE_TIME_MS 10

// communicate with AT24C164 serial eeprom(s)

/**
 * Start a write (or random read dummy) transaction with the
 * eeprom. If the eeprom is not responding, keep trying for up to the
 * eeprom write delay in case it is busy.
 */
static i2c_eeprom_err i2c_eeprom_start_write(void* addr){
	const intptr_t iaddr = (intptr_t) addr;

	// [ 1 | A2 | A1 | A0 | B2 | B1 | B0 | R/W ] A0-2 = device address, B0-2 = 3 MSB of 11-bit device address
	uint8_t address_byte = 0b10100000; // 010 address (all low) and write operation
	address_byte ^= ((iaddr >> 7) & 0b01111110); // select 14-bit device-and-address at once

	// the eeprom may be ignoring inputs because it's writing, so we
	// keep trying to issue our start condition for up to its write
	// time.  We can't use uptimems here, because with the v-usb model
	// that's not interrupt-fed, so just delay and check again

	uint8_t ack = 0;
	for(int i = 0; i < 11; ++i){ // wait up to 11ms
		twi_start();
		if(twi_write_byte(address_byte) == ACK) {
			ack = 1;
			break;
		}
		_delay_ms(1);
	}

	// If it timed out, return an error.
	if(!ack) {
		twi_stop(NOWAIT);
		return WSELECT_ERROR;
	}

	if(twi_write_byte(iaddr & 0xff) != ACK){
		twi_stop(NOWAIT);
		return ADDRESS_ERROR;
	}

	return SUCCESS;
}

/**
 * Continues writing an eeprom page-write transaction. Returns bytes
 * written: if < len, an error occurred.
 */
static uint8_t i2c_eeprom_continue_write(const uint8_t* buf, uint8_t len){
	int i = 0;
	for(; i < len; ++i){
		if(twi_write_byte(buf[i]) != ACK){
			storage_errno = DATA_ERROR;
			twi_stop(NOWAIT);
			break;
		}
	}
	return i;
}

static inline void i2c_eeprom_end_write(void){
	twi_stop(NOWAIT);
}

/**
 * Write len bytes within an eeprom page. The caller is responsible
 * for ensuring 0 < len <= 16 and aligned within the 16 byte page.
 * returns bytes written: if < len, an error occurred.
 */
static uint8_t i2c_eeprom_write_page(void* addr, const uint8_t* buf, uint8_t len){
	storage_errno = SUCCESS;

	uint8_t r = i2c_eeprom_start_write(addr);
	if(r != SUCCESS){
		storage_errno = r;
		return 0;
	}

	int8_t wr = i2c_eeprom_continue_write(buf, len);
	if(wr == len){
		// no error
		i2c_eeprom_end_write();
	}

	return wr;
}

/**
 * Writes count bytes to serial eeprom address dst, potentially using
 * multiple page writes. Returns number of bytes written if any bytes
 * were successfully written, otherwise -1. A return value of less
 * than count indicates that an error occurred and storage_errno
 * is set to indicate the error.
 */
int16_t i2c_eeprom_write(void* dst, const void* data, size_t count){
	int16_t written = 0;
	while(count){
		uint8_t dst_page_off = ((intptr_t) dst) & (EEEXT_PAGE_SIZE - 1);
		uint8_t dst_page_remaining = EEEXT_PAGE_SIZE - dst_page_off;
		uint8_t n = (count < dst_page_remaining) ? (uint8_t)count : dst_page_remaining;

		uint8_t w = i2c_eeprom_write_page(dst, (const uint8_t*)data, n);
		written += w;
		if(w != n){
			// error: incomplete write
			return (written == 0) ? written : -1;
		}
		data += n;
		dst += n;
		count -= n;
		USB_KeepAlive(true);
	}
	return written;
}

i2c_eeprom_err i2c_eeprom_write_byte(uint8_t* dst, uint8_t b){
	int r = i2c_eeprom_write_page(dst, &b, 1);
	if(r == 1){
		return SUCCESS;
	}
	else {
		return storage_errno;
	}
}

i2c_eeprom_err i2c_eeprom_write_short(uint8_t* dst, uint16_t s){
	int r = i2c_eeprom_write(dst, &s, sizeof(uint16_t));
	if(r == 1){
		return SUCCESS;
	}
	else {
		return storage_errno;
	}
}

/**
 * Repeatedly called to incrementally write chunks of data to eeprom.
 * The caller is responsible for ensuring that the range to be written
 * does not cross a page boundary, and that writing begins at a page
 * boundary.  Function automatically starts a page write if dst is at
 * a page boundary, and stops the page write after writing if dst+len
 * is a page boundary, or if 'last' is set.
 *
 * Returns i2c_eeprom_err.
 */
i2c_eeprom_err i2c_eeprom_write_step(void* dst, const void* data, uint8_t len, uint8_t last){
	i2c_eeprom_err r;
	if(((intptr_t)dst & (EEEXT_PAGE_SIZE-1)) == 0){
		// page aligned: start write
		r = i2c_eeprom_start_write(dst);
		if(r != SUCCESS) return r;
	}

	if(len != i2c_eeprom_continue_write((const uint8_t*) data, len)){
		return storage_errno;
	}

	intptr_t nextDst = (intptr_t) dst+len;
	if(last || ((nextDst & (EEEXT_PAGE_SIZE-1)) == 0)){
		i2c_eeprom_end_write();
	}

	return SUCCESS;
}

i2c_eeprom_err i2c_eeprom_memmove(void* dst, const void* src, size_t count){
	uint8_t buf[EEEXT_PAGE_SIZE];
	// copy in page aligned chunks

	int8_t direction;
	if(src < dst){
		direction = -1;
		src += count;
		dst += count; // Cursor points to just after the end of the array
	}
	else{
		direction = 1;
	}

	while(count){
		// offset into page
		uint8_t dst_cursor_page_offset = ((intptr_t) dst) & (EEEXT_PAGE_SIZE - 1);

		// either (0..page_offset) or (page_off..15) inclusive
		uint8_t dst_page_remaining;
		if(direction > 0){
			// copying forward from dst to the next page boundary
			dst_page_remaining = EEEXT_PAGE_SIZE - dst_cursor_page_offset;
		}
		else {
			// copying backward from dst-1 to the previous page boundary
            dst_page_remaining = dst_cursor_page_offset == 0 ? EEEXT_PAGE_SIZE : dst_cursor_page_offset;
		}

		uint8_t n = count < dst_page_remaining ? count : dst_page_remaining;

		if(direction < 0){
			src -= n;
			dst -= n;
		}
		if(i2c_eeprom_read(src, buf, n) != n){
			return storage_errno;
		}
		if(i2c_eeprom_write_page(dst, buf, n) != n){
			return storage_errno;
		}
		if(direction > 0){
			src += n;
			dst += n;
		}
		count -= n;
		USB_KeepAlive(true);
	}
	return SUCCESS;
}

i2c_eeprom_err i2c_eeprom_memset(void* dst, uint8_t c, size_t len){
	uint8_t buf[EEEXT_PAGE_SIZE];
	memset(buf, c, EEEXT_PAGE_SIZE);

	while(len > 0){
		uint8_t dst_page_off = ((intptr_t) dst) & (EEEXT_PAGE_SIZE - 1);
		uint8_t dst_page_remaining = EEEXT_PAGE_SIZE - dst_page_off;
		uint8_t step = (len < dst_page_remaining) ? (uint8_t)len : dst_page_remaining;

		uint8_t r = i2c_eeprom_write_page(dst, (const uint8_t*)buf, step);
		if(r != step){
			// error: incomplete write
			return storage_errno;
		}
		USB_KeepAlive(true);
		dst += step;
		len -= step;
	}
	return SUCCESS;
}

size_t i2c_eeprom_read(const void* addr, void* buf, size_t len){
	uint8_t* buf_bytes = (uint8_t*) buf;

	storage_errno = SUCCESS;
	size_t read_bytes = 0;

	// Set the current address by doing a "dummy write" to the address -
	// set up as though writing, but then don't send the actual byte
	uint8_t r = i2c_eeprom_start_write((void*) addr);
	if(r != SUCCESS){
		storage_errno = r;
		goto end;
	}

	// now send a new start condition, and the read command for the device address
	// (do not re-send the byte address)
	twi_start();

	uint8_t read_address = 0b10100001; // 010 address (all low) and read operation
	read_address ^= (((intptr_t)addr) >> 7) & 0b01111110; // select upper part of 14-bit device-and-address

	if(twi_write_byte(read_address) != ACK){
		storage_errno = RSELECT_ERROR; goto end;
	}

	// and start reading
	while(len--){
		*buf_bytes++ = twi_read_byte(len ? ACK : NACK); // nack on last byte to stop it talking to us
		++read_bytes;
	}

 end:
	twi_stop(NOWAIT);
	return read_bytes ? read_bytes : -1;
}

uint8_t i2c_eeprom_read_byte(const uint8_t* addr){
	uint8_t b;
	i2c_eeprom_read(addr, &b, sizeof(uint8_t));
	return b;
}

uint16_t i2c_eeprom_read_short(const uint16_t* addr){
	uint16_t s;
	i2c_eeprom_read(addr, &s, sizeof(uint16_t));
	return s;
}


#ifdef DEBUG

// test code to dump the contents of the eeprom - typically not linked in
uint8_t i2c_eeprom_test_read(void){
	static uint8_t* addr = 0;
	static uint8_t buf[8]; // read 8 at a time to prove multi-read
	static uint8_t bufp = sizeof(buf);

	uint8_t next_byte;
	if(bufp >= sizeof(buf)){
		int16_t r = i2c_eeprom_read(addr, buf, sizeof(buf));
		if(r != sizeof(buf)){
			addr = 0;
			switch(storage_errno){
			case RSELECT_ERROR:
				printing_set_buffer(CONST_MSG("RSELECT_ERROR"), CONSTANT_STORAGE);
				break;
			case WSELECT_ERROR:
				printing_set_buffer(CONST_MSG("WSELECT_ERROR"), CONSTANT_STORAGE);
				break;
			case ADDRESS_ERROR:
				printing_set_buffer(CONST_MSG("ADDRESS_ERROR"), CONSTANT_STORAGE);
				break;
			case DATA_ERROR:
				printing_set_buffer(CONST_MSG("DATA_ERROR"), CONSTANT_STORAGE);
				break;
			case SUCCESS:
				printing_set_buffer(CONST_MSG("SUCCESS:WHAT_ERROR?\n"), CONSTANT_STORAGE);
				break;
			default:
				printing_set_buffer(CONST_MSG("WTF_ERROR"), CONSTANT_STORAGE);
			}
			return 0;
		}
		addr += sizeof(buf);
		bufp = 0;
	}

	next_byte = buf[bufp++];

	printing_set_buffer(byte_to_str(next_byte), sram);
	return 1;
}

static uint16_t block[8] = { 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0e };
uint8_t i2c_eeprom_test_write(void){
	static uint8_t* addr = 0;

	int8_t r = i2c_eeprom_write_page(addr, (uint8_t*)block, 16);
	if(r != 16){
		addr = 0;
		switch(storage_errno){
		case RSELECT_ERROR:
			printing_set_buffer(CONST_MSG("RSELECT_ERROR\n"), CONSTANT_STORAGE);
			break;
		case WSELECT_ERROR:
			printing_set_buffer(CONST_MSG("WSELECT_ERROR\n"), CONSTANT_STORAGE);
			break;
		case ADDRESS_ERROR:
			printing_set_buffer(CONST_MSG("ADDRESS_ERROR\n"), CONSTANT_STORAGE);
			break;
		case DATA_ERROR:
			printing_set_buffer(CONST_MSG("DATA_ERROR\n"), CONSTANT_STORAGE);
			break;
		case SUCCESS:
			printing_set_buffer(CONST_MSG("SUCCESS:WHAT_ERROR?\n"), CONSTANT_STORAGE);
			break;
		default:
			printing_set_buffer(CONST_MSG("WTF_ERROR\n"), CONSTANT_STORAGE);
		}
		return 0;
	}
	else{
		printing_set_buffer(byte_to_str(((uint16_t)addr)/16), sram);
		addr += 16;
		for(int i = 0; i < 8; ++i){
			block[i] += 0x10;
		}
		return 1;
	}
}
#endif // DEBUG
