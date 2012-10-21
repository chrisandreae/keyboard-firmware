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

#include "keyboard.h"
#include "twi.h"
#include "printing.h"
#include "hardware.h"
#include "serial_eeprom.h"
#include "usb.h"

#include <util/delay.h>     /* for _delay_ms() */

#if USE_EEPROM

/* Serial eeprom support */

serial_eeprom_err serial_eeprom_errno = SUCCESS;

#define SERIAL_EEPROM_WRITE_TIME_MS 10

// communicate with AT24C164 serial eeprom(s)

/**
 * Start a write (or random read dummy) transaction with the
 * eeprom. If the eeprom is not responding, keep trying for up to the
 * eeprom write delay in case it is busy.
 */
serial_eeprom_err serial_eeprom_start_write(uint8_t* addr){
	const uint16_t iaddr = (uint16_t) addr;

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
		twi_stop();
		return WSELECT_ERROR;
	}

	if(twi_write_byte(iaddr & 0xff) != ACK){
		twi_stop();
		return ADDRESS_ERROR;
	}

	return SUCCESS;
}

/**
 * Continues writing an eeprom page-write transaction. Returns bytes
 * written: if < len, an error occurred.
 */
int8_t serial_eeprom_continue_write(const uint8_t* buf, uint8_t len){
	int i = 0;
	for(; i < len; ++i){
		if(twi_write_byte(buf[i]) != ACK){
			serial_eeprom_errno = DATA_ERROR;
			twi_stop();
			break;
		}
	}
	return i;
}

/**
 * Write len bytes within an eeprom page. The caller is responsible
 * for ensuring 0 < len <= 16 and aligned within the 16 byte page.
 * returns bytes written: if < len, an error occurred.
 */
int8_t serial_eeprom_write_page(uint8_t* addr, const uint8_t* buf, uint8_t len){
	serial_eeprom_errno = SUCCESS;

	uint8_t r = serial_eeprom_start_write(addr);
	if(r != SUCCESS){
		serial_eeprom_errno = r;
		return 0;
	}

	int8_t wr = serial_eeprom_continue_write(buf, len);
	if(wr == len){
		// no error
		serial_eeprom_end_write();
	}

	return wr;
}

/**
 * Writes count bytes to serial eeprom address dst, potentially using
 * multiple page writes. Returns number of bytes written if any bytes
 * were successfully written, otherwise -1. A return value of less
 * than count indicates that an error occurred and serial_eeprom_errno
 * is set to indicate the error.
 */
int16_t serial_eeprom_write(uint8_t* dst, const uint8_t* buf, uint16_t count){
	int16_t written = 0;
	while(count){
		uint8_t dst_page_off = ((intptr_t) dst) & (EEEXT_PAGE_SIZE - 1);
		uint8_t dst_page_remaining = EEEXT_PAGE_SIZE - dst_page_off;
		uint8_t n = (count < dst_page_remaining) ? (uint8_t)count : dst_page_remaining;

		int8_t w = serial_eeprom_write_page(dst, buf, n);
		written += w;
		if(w != n){
			// error: incomplete write
			return (written == 0) ? written : -1;
		}
		buf += n;
		dst += n;
		count -= n;
		USB_KeepAlive(true);
	}
	return written;
}

/**
 * Repeatedly called to incrementally write chunks of data to eeprom.
 * The caller is responsible for ensuring that the range to be written
 * does not cross a page boundary, and that writing begins at a page
 * boundary.  Function automatically starts a page write if addr is at
 * a page boundary, and stops the page write after writing if addr+len
 * is a page boundary, or if 'last' is set.
 *
 * Returns serial_eeprom_err.
 */
serial_eeprom_err serial_eeprom_write_step(uint8_t* addr, uint8_t* data, uint8_t len, uint8_t last){
	serial_eeprom_err r;
	if(((intptr_t)addr & (EEEXT_PAGE_SIZE-1)) == 0){
		// page aligned: start write
		r = serial_eeprom_start_write(addr);
		if(r != SUCCESS) return r;
	}

	if(len != serial_eeprom_continue_write(data, len)){
		return serial_eeprom_errno;
	}

	intptr_t nextAddr = (intptr_t) addr+len;
	if(last || ((nextAddr & (EEEXT_PAGE_SIZE-1)) == 0)){
		serial_eeprom_end_write();
	}

	return SUCCESS;
}

serial_eeprom_err serial_eeprom_memmove(uint8_t* dst, uint8_t* src, size_t count){
	uint8_t buf[EEEXT_PAGE_SIZE];
	// copy in page aligned chunks

	int8_t direction = src < dst ? -1 : 1;

	while(count){
		// offset into page
		uint8_t dst_page_off = ((intptr_t) dst) & (EEEXT_PAGE_SIZE - 1);
		// either (0..page_off) or (page_off..15) inclusive
		uint8_t dst_page_remaining = direction > 0 ? (EEEXT_PAGE_SIZE - dst_page_off) : (dst_page_off + 1);
		uint8_t n = count < dst_page_remaining ? count : dst_page_remaining;
		if(direction < 0){
			src -= n;
			dst -= n;
		}
		if(serial_eeprom_read(src, buf, n) != n){
			return serial_eeprom_errno;
		}
		if(serial_eeprom_write_page(dst, buf, n) != n){
			return serial_eeprom_errno;
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


int16_t serial_eeprom_read(const uint8_t* addr, uint8_t* buf, uint16_t len){
	serial_eeprom_errno = SUCCESS;
	uint8_t read_bytes = 0;

	// Set the current address by doing a "dummy write" to the address -
	// set up as though writing, but then don't send the actual byte
	uint8_t r = serial_eeprom_start_write((uint8_t*) addr);
	if(r != SUCCESS){
		serial_eeprom_errno = r;
		goto end;
	}

	// now send a new start condition, and the read command for the device address
	// (do not re-send the byte address)
	twi_start();

	uint8_t read_address = 0b10100001; // 010 address (all low) and read operation
	read_address ^= (((uint16_t)addr) >> 7) & 0b01111110; // select upper part of 14-bit device-and-address

	if(twi_write_byte(read_address) != ACK){
		serial_eeprom_errno = RSELECT_ERROR; goto end;
	}

	// and start reading
	while(len--){
		*buf++ = twi_read_byte(len ? ACK : NACK); // nack on last byte to stop it talking to us
		++read_bytes;
	}

 end:
	twi_stop();
	return read_bytes ? read_bytes : -1;
}

#ifdef DEBUG

// test code to dump the contents of the eeprom - typically not linked in
uint8_t serial_eeprom_test_read(void){
	static uint8_t* addr = 0;
	static uint8_t buf[8]; // read 8 at a time to prove multi-read
	static uint8_t bufp = sizeof(buf);

	uint8_t next_byte;
	if(bufp >= sizeof(buf)){
		int16_t r = serial_eeprom_read(addr, buf, sizeof(buf));
		if(r != sizeof(buf)){
			addr = 0;
			switch(serial_eeprom_errno){
			case RSELECT_ERROR:
				printing_set_buffer(PGM_MSG("RSELECT_ERROR"), BUF_PGM);
				break;
			case WSELECT_ERROR:
				printing_set_buffer(PGM_MSG("WSELECT_ERROR"), BUF_PGM);
				break;
			case ADDRESS_ERROR:
				printing_set_buffer(PGM_MSG("ADDRESS_ERROR"), BUF_PGM);
				break;
			case DATA_ERROR:
				printing_set_buffer(PGM_MSG("DATA_ERROR"), BUF_PGM);
				break;
			case SUCCESS:
				printing_set_buffer(PGM_MSG("SUCCESS:WHAT_ERROR?\n"), BUF_PGM);
				break;
			default:
				printing_set_buffer(PGM_MSG("WTF_ERROR"), BUF_PGM);
			}
			return 0;
		}
		addr += sizeof(buf);
		bufp = 0;
	}

	next_byte = buf[bufp++];

	printing_set_buffer(byte_to_str(next_byte), BUF_MEM);
	return 1;
}

static uint16_t block[8] = { 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0e };
uint8_t serial_eeprom_test_write(void){
	static uint8_t* addr = 0;

	int8_t r = serial_eeprom_write_page(addr, (uint8_t*)block, 16);
	if(r != 16){
		addr = 0;
		switch(serial_eeprom_errno){
		case RSELECT_ERROR:
			printing_set_buffer(PGM_MSG("RSELECT_ERROR\n"), BUF_PGM);
			break;
		case WSELECT_ERROR:
			printing_set_buffer(PGM_MSG("WSELECT_ERROR\n"), BUF_PGM);
			break;
		case ADDRESS_ERROR:
			printing_set_buffer(PGM_MSG("ADDRESS_ERROR\n"), BUF_PGM);
			break;
		case DATA_ERROR:
			printing_set_buffer(PGM_MSG("DATA_ERROR\n"), BUF_PGM);
			break;
		case SUCCESS:
			printing_set_buffer(PGM_MSG("SUCCESS:WHAT_ERROR?\n"), BUF_PGM);
			break;
		default:
			printing_set_buffer(PGM_MSG("WTF_ERROR\n"), BUF_PGM);
		}
		return 0;
	}
	else{
		printing_set_buffer(byte_to_str(((uint16_t)addr)/16), BUF_MEM);
		addr += 16;
		for(int i = 0; i < 8; ++i){
			block[i] += 0x10;
		}
		return 1;
	}
}
#endif // DEBUG

#endif
