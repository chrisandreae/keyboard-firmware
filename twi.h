#ifndef __TWI_H
#define __TWI_H

#include "kinesis.h"

#ifdef USE_EEPROM

typedef enum _twi_ack {
	ACK = 0,
	NACK = 1
} twi_ack;

void twi_start(void);
void twi_stop(void);
uint8_t twi_read_byte(twi_ack ack);
twi_ack twi_write_byte(uint8_t val);

#endif

#endif
