#ifndef __AVR_EEPROM_H
#define __AVR_EEPROM_H

#include "storage.h"

#include <avr/eeprom.h>

#define STORAGE_SECTION_avr_eeprom EEMEM

inline int16_t avr_eeprom_write(void* dst, const void* data, size_t count){
	eeprom_update_block(data, dst, count);
	return count;
}

inline storage_err avr_eeprom_write_byte(uint8_t* dst, uint8_t b){
	eeprom_update_byte(dst, b);
	return 0;
}

inline storage_err avr_eeprom_write_short(uint16_t* dst, uint16_t v){
	eeprom_update_word(dst, v);
	return 0;
}

inline storage_err avr_eeprom_write_step(void* dst, const void* data, uint8_t len, uint8_t last){
	eeprom_update_block(data, dst, len);
	return 0;
}

inline size_t avr_eeprom_read(const void* addr, void* buf, size_t len){
	eeprom_read_block(buf, addr, len);
	return len;
}

inline uint8_t avr_eeprom_read_byte(const uint8_t* addr){
	return eeprom_read_byte(addr);
}

inline uint16_t avr_eeprom_read_short(const uint16_t* addr){
	return eeprom_read_word(addr);
}

storage_err avr_eeprom_memmove(void* dst, const void* src, size_t count);

storage_err avr_eeprom_memset(void* dst, uint8_t c, size_t len);

#endif // __AVR_EEPROM_H
