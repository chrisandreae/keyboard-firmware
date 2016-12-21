#include "storage/avr_pgm.h"

// Ensure symbols are generated for inline functions defined in header
size_t avr_pgm_read(const void* src, void* dst, size_t len);
uint8_t avr_pgm_read_byte(const uint8_t* addr);
uint16_t avr_pgm_read_short(const uint16_t* addr);
