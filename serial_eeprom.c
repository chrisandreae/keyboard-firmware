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
#include "serial_eeprom.h"
#include "twi.h"
#include "printing.h"
#include "hardware.h"

#ifdef USE_EEPROM

/* Serial eeprom support */

static const uint8_t SERIAL_EEPROM_WRITE_TIME_MS = 10;

static enum { SUCCESS, WSELECT_ERROR, RSELECT_ERROR, ADDRESS_ERROR, DATA_ERROR } serial_eeprom_errno = SUCCESS;

// communicate with AT24C164 serial eeprom(s)
uint8_t serial_eeprom_write_byte(uint16_t addr, uint8_t data){
	serial_eeprom_errno = SUCCESS;

	twi_start();

	// [ 1 | A2 | A1 | A0 | B2 | B1 | B0 | R/W ] A0-2 = device address, B0-2 = 3 MSB of 11-bit device address
	uint8_t address_byte = 0b10100000; // 010 address (all low) and write operation
	address_byte ^= ((addr >> 7) & 0b01111110); // select 14-bit device-and-address at once

	if(twi_write_byte(address_byte) != ACK){
		serial_eeprom_errno = WSELECT_ERROR; goto fail;
	}
	if(twi_write_byte(addr & 0xff) != ACK){
		serial_eeprom_errno = ADDRESS_ERROR; goto fail;
	}
	if(twi_write_byte(data) != ACK){
		serial_eeprom_errno = DATA_ERROR; goto fail;
	}

 fail:
	twi_stop();
	return serial_eeprom_errno == SUCCESS;
}

int16_t serial_eeprom_write(uint16_t addr, uint8_t* buf, int16_t len){
	// todo: support page write to save rewrite cycles
	serial_eeprom_errno = SUCCESS;

	for(int i = 0; i < len; ++i){
		if(!serial_eeprom_write_byte(addr+i, buf[i])){
			if(i) return i;
			else return -1;
		}
	}
	return len;
}

int16_t serial_eeprom_read(uint16_t addr, uint8_t* buf, int16_t len){
	serial_eeprom_errno = SUCCESS;
	uint8_t read_bytes = 0;

	twi_start();

	// Set the current address by doing a "dummy write" to the address -
	// set up as though writing, but then don't send the actual byte

	// [ 1 | A2 | A1 | A0 | B2 | B1 | B0 | R/W ] A0-2 = device address, B0-2 = 3 MSB of 11-bit device address
	uint8_t address_byte = 0b10100000; // 010 address (all low) and write operation
	address_byte ^= ((addr >> 7) & 0b01111110); // select 14-bit device-and-address at once

	if(twi_write_byte(address_byte) != ACK){
		serial_eeprom_errno = WSELECT_ERROR; goto fail;
	}
	if(twi_write_byte(addr & 0xff) != ACK){
		serial_eeprom_errno = ADDRESS_ERROR; goto fail;
	}

	// then send a start again, and the address with the read bit set.
	twi_start();

	uint8_t read_addr = address_byte | 0x1; // do I need to clip out the high bits here?
	if(twi_write_byte(read_addr) != ACK){
		serial_eeprom_errno = RSELECT_ERROR; goto fail;
	}

	while(len--){
		*buf++ = twi_read_byte(len ? ACK : NACK); // nack on last byte to stop it talking to us
		++read_bytes;
	}

 fail:
	twi_stop();
	return read_bytes ? read_bytes : -1;
}

uint8_t serial_eeprom_test_read(void){
	static uint16_t addr = 0;
	uint8_t b;

	int16_t r = serial_eeprom_read(addr++, &b, 1);
	if(r != 1){
		switch(serial_eeprom_errno){
		case RSELECT_ERROR:{
			static const char msg[] PROGMEM = "RSELECT_ERROR";
			printing_set_buffer(msg);
			break;
		}
		case WSELECT_ERROR:{
			static const char msg[] PROGMEM = "WSELECT_ERROR";
			printing_set_buffer(msg);
			break;
		}
		case ADDRESS_ERROR:{
			static const char msg[] PROGMEM = "ADDRESS_ERROR";
			printing_set_buffer(msg);
			break;
		}
		case DATA_ERROR:{
			static const char msg[] PROGMEM = "DATA_ERROR";
			printing_set_buffer(msg);
			break;
		}
		default:{
			static const char msg[] PROGMEM = "WTF_ERROR";
			printing_set_buffer(msg);
		}
		}
		return 0;
	}
	else{
		printing_set_buffer(print_byte(b));
		return 1;
	}
}


#endif
