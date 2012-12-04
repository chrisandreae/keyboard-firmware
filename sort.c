/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  Licensed under the GNU GPL v2 (see GPL2.txt).

 *  -----
 * Components of this file are
 *
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ronnie Kon at Mindcraft Inc., Kevin Lew and Elmer Yglesias.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdint.h>
#include <stddef.h>

/*
 * Swap two areas of size number of bytes.  Although qsort(3) permits random
 * blocks of memory to be sorted, sorting pointers is almost certainly the
 * common case (and, were it not, could easily be made so).  Regardless, it
 * isn't worth optimizing; the SWAP's get sped up by the cache, and pointer
 * arithmetic gets lost in the time required for comparison function calls.
 */
#define	SWAP(a, b, tmp) { \
		tmp = *a;		  \
		*a++ = *b;		  \
		*b++ = tmp;		  \
	}

/* Copy one block to another. */
#define COPY(a, b) { \
		*a = *b;	 \
	}

#define compar(a, b) (*(a) - *(b))

/*
 * Heapsort -- Knuth, Vol. 3, page 145.  Runs in O (N lg N), both average
 * and worst.  While heapsort is faster than the worst case of quicksort,
 * the BSD quicksort does median selection so that the chance of finding
 * a data set that will trigger the worst case is nonexistent.  Heapsort's
 * only advantage over quicksort is that it requires little additional memory.
 *
 * Hacked for unsigned byte comparison.
 */
typedef uint8_t elt_t;
int heapsort_uint8(elt_t* vbase, size_t nmemb){
	elt_t *base;
	elt_t k, tmp;

	if (nmemb <= 1)
		return (0);

	/*
	 * Items are numbered from 1 to nmemb, so offset 1 element below
	 * the starting address.
	 */
	base = vbase - 1;

	for (uint16_t initval = nmemb / 2 + 1; --initval; ){
		/*
		 * Build the list into a heap, where a heap is defined such that for
		 * the records K1 ... KN, Kj/2 >= Kj for 1 <= j/2 <= j <= N.
		 *
		 * There two cases.  If j == nmemb, select largest of Ki and Kj.  If
		 * j < nmemb, select largest of Ki, Kj and Kj+1.
		 */
		uint16_t par_i, child_i;
		for (par_i = initval; (child_i = par_i * 2) <= nmemb; par_i = child_i) {
			elt_t* child = &base[child_i];
			if (child_i < nmemb && compar(child, child + 1) < 0) {
				++child;
				++child_i;
			}
			elt_t* par = &base[par_i];
			if (compar(child, par) <= 0)
				break;
			SWAP(par, child, tmp);
		}
	}

	/*
	 * For each element of the heap, save the largest element into its
	 * final slot, save the displaced element (k), then recreate the
	 * heap.
	 */
	while (nmemb > 1) {
		k = base[nmemb];  // COPY(k, base+nmemb*size ...)
		base[nmemb] = base[1]; // COPY(base+nmemb*size, base+size ...)
		--nmemb;

		{
			/*
			 * Select the top of the heap and 'heapify'.  Since by far the most expensive
			 * action is the call to the compar function, a considerable optimization
			 * in the average case can be achieved due to the fact that k, the displaced
			 * elememt, is usually quite small, so it would be preferable to first
			 * heapify, always maintaining the invariant that the larger child is copied
			 * over its parent's record.
			 *
			 * Then, starting from the *bottom* of the heap, finding k's correct place,
			 * again maintianing the invariant.  As a result of the invariant no element
			 * is 'lost' when k is assigned its correct place in the heap.
			 *
			 * The time savings from this optimization are on the order of 15-20% for the
			 * average case. See Knuth, Vol. 3, page 158, problem 18.
			 */
			uint16_t par_i, child_i;
			for (par_i = 1; (child_i = par_i * 2) <= nmemb; par_i = child_i) {
				elt_t* child = &base[child_i];
					if (child_i < nmemb && compar(child, child + 1) < 0) {
						++child;
						++child_i;
					}
				elt_t* par = &base[par_i];
				COPY(par, child);
			}
			for (;;) {
				child_i = par_i;
				par_i = child_i / 2;
				elt_t* child = &base[child_i];
				elt_t* par   = &base[par_i];
				if (child_i == 1 || compar(&k, par) < 0) {
					COPY(child, &k);
					break;
				}
				COPY(child, par);
			}
		}
	}
	return (0);
}

void insertionsort_uint8(uint8_t* base, size_t nmemb){
	for(size_t i = 1; i < nmemb; ++i){
		uint8_t val = base[i];
		// insert into [0 .. i-1], moving up until <=val
		size_t j;
		for(j = i; j > 0 && base[j-1] > val; --j){
			base[j] = base[j-1];
		}
		base[j] = val;
	}
}

#ifdef DEBUG
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int cmp(const void* a, const void* b){
	const uint8_t* aa = (const uint8_t*) a;
	const uint8_t* bb = (const uint8_t*) b;
	return (*aa - *bb);
}

int main(){
	const uint8_t data[] = {
		0x79, 0xf1, 0x9b, 0x04, 0x64, 0x02, 0x11, 0x38, 0x8c, 0xc2, 0xc5, 0x96, 0x7f, 0x41, 0x4d, 0x5c,
		0x64, 0xc0, 0x6c, 0x78, 0x73, 0x7e, 0x42, 0x78, 0xb7, 0x48, 0x6b, 0x1f, 0x5d, 0xd4, 0x3c, 0x46,
		0xd8, 0xd6, 0xc7, 0x40, 0x3e, 0x1a, 0x11, 0xdf, 0x7f, 0xf7, 0x3c, 0xa2, 0x02, 0x32, 0x11, 0xe6,
		0xee, 0xbb, 0xf1, 0x5a, 0xba, 0x53, 0x5d, 0xb4, 0x33, 0x7e, 0x40, 0xac, 0x8e, 0x7d, 0x14, 0x78,
		0x84, 0x19, 0x23, 0x24, 0x68, 0x1a, 0x5e, 0x36, 0xfe, 0x47, 0xaa, 0x4f, 0x45, 0xe8, 0xbd, 0x91,
		0x03, 0xaf, 0x50, 0x00, 0x97, 0x91, 0xe8, 0x05, 0x80, 0x9d, 0xf0, 0xb7, 0xbd, 0x92, 0x6e, 0xd6,
		0x78, 0x60, 0x96, 0x5a, 0x98, 0xaf, 0xa8, 0xa9, 0x5b, 0xdc, 0x69, 0x39, 0x0f, 0xd1, 0x05, 0x38,
		0xd1, 0x32, 0x20, 0x68, 0x19, 0x2c, 0xbe, 0x76, 0xc2, 0x81, 0x54, 0x8d, 0x39, 0x06, 0xa6, 0x4c,
		0xa0, 0xb6, 0xf8, 0x8e, 0x2c, 0x8d, 0x61, 0x88, 0x22, 0x59, 0xa7, 0xd6, 0x10, 0x5a, 0x95, 0x61,
		0x46, 0x03, 0x05, 0x06, 0x85, 0x25, 0xb2, 0x6f, 0xa7, 0x16, 0xba, 0x6d, 0x79, 0x3e, 0x32, 0x07,
		0x75, 0xff, 0x15, 0xf3, 0x08, 0x3b, 0x5f, 0x2f, 0xe9, 0x39, 0xcf, 0xe6, 0x3c, 0x9d, 0x1b, 0x9d,
		0x13, 0x10, 0x82, 0x91, 0x33, 0x7b, 0x06, 0xce, 0x2c, 0x12, 0xe9, 0x4b, 0x10, 0xdc, 0x23, 0xef,
		0xe0, 0xca, 0x11, 0x1f, 0xde, 0xc6, 0x3d, 0xff, 0xdc, 0xe2, 0xe9, 0x2e, 0xcd, 0xfa, 0xc5, 0xf5,
		0x55, 0x38, 0xf6, 0x7e, 0xb6, 0xcb, 0x59, 0xf2, 0x67, 0x90, 0xb5, 0x83, 0xc5, 0x80, 0x8d, 0x9f,
		0xd0, 0xac, 0xd5, 0x0c, 0xb7, 0x75, 0x8e, 0x9b, 0x45, 0xc2, 0xc7, 0xc1, 0x03, 0x10, 0x4c, 0x54,
		0xfd, 0x32, 0x54, 0x91, 0xa7, 0xcd, 0xb7, 0x72, 0x75, 0x4f, 0x5c, 0xa5, 0x12, 0xbc, 0xf7, 0x17,
		0x79, 0x56, 0x15, 0x39, 0x1b, 0xcd, 0x7d, 0x35, 0x5c, 0x83, 0xcf, 0x87, 0x23, 0xcb, 0xdc, 0x23,
		0xa8, 0xa3, 0x1b, 0xa4, 0xa9, 0xbe, 0xcc, 0xfd, 0xfd, 0x7f, 0x3a, 0x24, 0x2b, 0x27, 0x27, 0x27,
		0xad, 0xdf, 0x41, 0x30, 0x7e, 0x0d, 0xed, 0x38, 0x53, 0x99, 0x50, 0x07, 0x07, 0x7c, 0x61, 
	};

	uint8_t* d1 = malloc(sizeof(data));
	uint8_t* d2 = malloc(sizeof(data));
	uint8_t* d3 = malloc(sizeof(data));
	memcpy(d1, data, sizeof(data));
	memcpy(d2, data, sizeof(data));
	memcpy(d3, data, sizeof(data));

	heapsort_uint8(d1, sizeof(data));
	insertionsort_uint8(d2, sizeof(data));
	qsort(d3, sizeof(data), 1, &cmp);

	int i = memcmp(d1, d3, sizeof(data));
	int j = memcmp(d2, d3, sizeof(data));
	printf("Compare: %d, %d\n", i, j);

	/* for(int i = 0; i < sizeof(data); ++i){ */
		/* printf("0x%x . 0x%x . 0x%x\n", d1[i], d2[i], d3[i]); */
	/* } */
}

#endif
