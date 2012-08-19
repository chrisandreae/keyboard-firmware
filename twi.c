#include <util/delay.h>
#include "twi.h"

#ifdef USE_EEPROM

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

#endif
