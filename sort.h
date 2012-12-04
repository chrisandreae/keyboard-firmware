/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  Licensed under the GNU GPL v2 (see GPL2.txt).
*/

#ifndef __SORT_H
#define __SORT_H

#include <stdint.h>
#include <stddef.h>

void insertionsort_uint8(uint8_t* base, size_t nmemb);

int heapsort_uint8(uint8_t* vbase, size_t nmemb);

#endif // __SORT_H
