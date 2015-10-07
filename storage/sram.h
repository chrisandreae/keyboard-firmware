#ifndef __SRAM_H
#define __SRAM_H

#include "storage.h"

#include <string.h>

#define STORAGE_SECTION_sram

inline int16_t sram_write(void* dst, const void* data, size_t count){
	memcpy(dst, data, count);
	return count;
}

inline storage_err sram_write_byte(uint8_t* dst, uint8_t b){
	*dst = b;
	return 0;
}

inline storage_err sram_write_short(uint16_t* dst, uint16_t v){
	*dst = v;
	return 0;
}

inline storage_err sram_write_step(uint8_t* dst, const uint8_t* data, uint8_t len, uint8_t last){
	memcpy(dst, data, len);
	return len;
}

inline int16_t sram_read(const void* addr, void* buf, size_t len){
	memcpy(buf, addr, len);
	return len;
}

inline uint8_t sram_read_byte(const uint8_t* addr){
	return *addr;
}

inline uint8_t sram_read_short(const uint16_t* addr){
	return *addr;
}

inline storage_err sram_memmove(void* dst, void* src, size_t count){
	memmove(dst, src, count);
	return 0;
}

inline storage_err sram_memset(void* dst, uint8_t c, size_t len){
	memset(dst, c, len);
	return 0;
}

#endif // __SRAM_H
