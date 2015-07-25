#ifndef __AVR_PGM_H
#define __AVR_PGM_H

#include "storage.h"

#include <avr/pgmspace.h>

#define STORAGE_SECTION_avr_pgm PROGMEM

inline size_t avr_pgm_read(const void* src, void* dst, size_t len){
	const uint8_t* src_bytes = (const uint8_t*) src;
	uint8_t* dst_bytes = (uint8_t*) dst;

	for(size_t i = 0; i < len; ++i){
		dst_bytes[i] = pgm_read_byte_near(&src_bytes[i]);
	}
	return len;
}

inline uint8_t avr_pgm_read_byte(const uint8_t* addr){
	return pgm_read_byte_near(addr);
}

inline uint16_t avr_pgm_read_short(const uint16_t* addr){
	return pgm_read_word_near(addr);
}

#endif // __AVR_PGM_H
