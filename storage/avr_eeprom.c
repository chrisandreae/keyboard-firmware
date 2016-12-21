#include "storage/avr_eeprom.h"
#include <avr/eeprom.h>
#include <stdlib.h>

// Ensure symbols are generated for inline functions defined in header
int16_t avr_eeprom_write(void* dst, const void* data, size_t count);
storage_err avr_eeprom_write_byte(uint8_t* dst, uint8_t b);
storage_err avr_eeprom_write_short(uint16_t* dst, uint16_t v);
storage_err avr_eeprom_write_step(void* dst, const void* data, uint8_t len, uint8_t last);
size_t avr_eeprom_read(const void* addr, void* buf, size_t len);
uint8_t avr_eeprom_read_byte(const uint8_t* addr);
uint16_t avr_eeprom_read_short(const uint16_t* addr);

storage_err avr_eeprom_memmove(void* dst, const void* src, size_t count){
	const uint8_t* src_bytes = (const uint8_t*) src;
	uint8_t*       dst_bytes = (uint8_t*) dst;

	int8_t direction;
	if(src_bytes < dst_bytes){
		direction = -1;
		dst_bytes += (count - 1);
		src_bytes += (count - 1);
	}
	else{
		direction = 1;
	}
	while(count){
		avr_eeprom_write_byte(dst_bytes, avr_eeprom_read_byte(src_bytes));
		src_bytes += direction;
		dst_bytes += direction;
		--count;
	}
	return 0;
}

storage_err avr_eeprom_memset(void* dst, uint8_t c, size_t len){
	uint8_t* dst_bytes = (uint8_t*) dst;

	for(size_t i = 0; i < len; ++i){
		eeprom_update_byte(&dst_bytes[i], c);
	}
	return 0;
}
