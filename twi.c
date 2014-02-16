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

#include <util/delay.h>
#include "twi.h"

#ifndef BITBANG_TWI
#include <util/twi.h>

void twi_init(void) {
	// 0 prescaler
	TWSR &= ~( (1<<TWPS1)|(1<<TWPS0) );
	// set the bit rate based on TWI_FREQ
	// twifreq = F_CPU/(16 + 2 * TWBR * (4^prescaler))
	TWBR = ((F_CPU / TWI_FREQ) - 16) / 2;
}

void twi_start(){
	TWCR = (1<<TWINT) | (1<<TWSTA) | (1<<TWEN);
	while ((TWCR & (1<<TWINT)) == 0);
}
void twi_stop(twi_wait wait){
	TWCR = (1<<TWINT) | (1<<TWSTO) | (1<<TWEN);
	if(wait == WAIT){
		while (TWCR & (1<<TWSTO));
	}
}
uint8_t twi_read_byte(twi_ack ack){
	TWCR = (1<<TWINT) | (1<<TWEN);
	if(ack == ACK){
		TWCR |= (1<<TWEA);
	}
	while ((TWCR & (1<<TWINT)) == 0);
	return TWDR;
}

twi_ack twi_write_byte(uint8_t val){
	TWDR = val;
	TWCR = (1<<TWINT)|(1<<TWEN);
	while ((TWCR & (1<<TWINT)) == 0);

	// upper 5 bits from TWSR register describe our whole status
	// We only care whether we have ack or nack-or-fail
	// after some type of send - SLA+R, SLA+W or data
	uint8_t status = TW_STATUS;
	// our compatible API only cares about ack/nack
	if(status == TW_MT_DATA_ACK || // 0x28
	   status == TW_MT_SLA_ACK  || // 0x18
	   status == TW_MR_SLA_ACK)    // 0x40
		return ACK;
	else
		return NACK;
}

#else // bitbang TWI

// At ideal voltage, can clock at up to 400khz (i.e 2.5 us per clock). Be slightly slower.
#define TWI_CLOCK_US 4

// two-wire interface to serial eeprom
static inline void twi_sda_high(void){
	EEPROM_DDR &= ~EEPROM_SDA;
}
static inline void twi_sda_low(void){
	EEPROM_DDR |= EEPROM_SDA;
}
static inline void twi_scl_high(void){
	EEPROM_DDR &= ~EEPROM_SCL;
}
static inline void twi_scl_low(void){
	EEPROM_DDR |= EEPROM_SCL;
}

void twi_init(void){
	// Serial eeprom lines have external pull-ups, so 0 = output-low(1,0) / 1 = input-highz(0,0)
	EEPROM_PORT &= ~(EEPROM_SCL | EEPROM_SDA); // initially leave high-z
	EEPROM_DDR  &= ~(EEPROM_SCL | EEPROM_SDA);
}

void twi_start(){
	// Ensure bus is idle (SCL/SDA high)
	EEPROM_DDR &= ~(EEPROM_SDA | EEPROM_SCL);
	_delay_us(TWI_CLOCK_US);

	// bring SDA high-to-low with SCL high for the start condition
	twi_sda_low();
	_delay_us(TWI_CLOCK_US);

	// Then lower the clock so we're ready to write.
	twi_scl_low();
}

void twi_stop(){
	// precondition: SCL low, may change SDA then delay before clock up.
	twi_sda_low();
	_delay_us(TWI_CLOCK_US);

	// raise clock
	twi_scl_high();
	_delay_us(TWI_CLOCK_US);

	// then bring SDA low-to-high with SCL high to signal stop condition
	twi_sda_high();
	_delay_us(TWI_CLOCK_US);
}

// After we try to let the clock float high, the device may continue holding it low
// to say that it's not ready for the next bit yet. If this is the case, wait until
// it's released.
static inline void twi_clock_stretch(){
	while((EEPROM_PIN & EEPROM_SCL) == 0);
}

void twi_write_bit(uint8_t b){
	// precondition: SCL low, may change SDA then delay before clock up.

	// Write the next bit into SDA ready to be clocked in (we may only change SDA whicle SCL is low)
	if(b) twi_sda_high();
	else twi_sda_low();

	_delay_us(TWI_CLOCK_US);

	twi_scl_high();
	twi_clock_stretch();
	_delay_us(TWI_CLOCK_US);

	twi_scl_low();

}

uint8_t twi_read_bit(){
	// precondition: SCL low, may change SDA then delay before clock up.

	twi_sda_high(); // ensure that data line is high
	_delay_us(TWI_CLOCK_US);

	twi_scl_high();
	twi_clock_stretch();
	_delay_us(TWI_CLOCK_US);

	// read at the negative clock edge
	uint8_t c = (EEPROM_PIN & EEPROM_SDA) ? 1 : 0;

	twi_scl_low();

	return c;
}

twi_ack twi_write_byte(uint8_t b){
	// send 8 bits
	for(uint8_t i = 0; i < 8; ++i){
		twi_write_bit(b & 128);
		b <<= 1;
	}
	// and read acknowledgment bit
	return twi_read_bit();
}

uint8_t twi_read_byte(twi_ack ack){
	uint8_t r = 0;
	for(uint8_t i = 0; i < 8; ++i){
		r = (r << 1) | twi_read_bit();
	}
	// transmit ack
	twi_write_bit(ack);
	return r;
}

#endif // BITBANG_TWI
