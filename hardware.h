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

#ifndef __HARDWARE_H
#define __HARDWARE_H

#define KINESIS    1
#define KINESIS110 2
#define ERGODOX    3
#define SPLITBOARD 4
#define FOURBYFOUR 5

// Select the specific keyboard hardware
#if HARDWARE_VARIANT == KINESIS
	#include "hardware/kinesis.h"
#elif HARDWARE_VARIANT == KINESIS110
	#include "hardware/kinesis110.h"
#elif HARDWARE_VARIANT == ERGODOX
	#include "hardware/ergodox.h"
#elif HARDWARE_VARIANT == SPLITBOARD
	#include "hardware/splitboard.h"
#elif HARDWARE_VARIANT == FOURBYFOUR
	#include "hardware/4x4.h"
#else
	#error "Unknown hardware variant selected"
#endif

// relative sort order of reset config and reset fully
#if SPECIAL_HKEY_RESET_CONFIG < SPECIAL_HKEY_RESET_FULLY
	#define SPECIAL_HKEY_RESET_CONFIG_POS 0
#else
    #define SPECIAL_HKEY_RESET_CONFIG_POS 1
#endif

// Ensure that storage locations are correctly set up
#ifndef CONSTANT_STORAGE
#error "Constant data storage location not defined"
#endif

#ifndef MAPPING_STORAGE
#error "Key mapping data storage location not defined"
#endif

#ifndef SAVED_MAPPING_STORAGE
#error "Saved mappings data storage location not defined"
#endif

#ifndef SAVED_MAPPING_COUNT
#error "Saved mappings count not defined"
#endif

#ifndef MACRO_INDEX_STORAGE
#error "Macro index data storage not defined"
#endif

#ifndef MACRO_INDEX_COUNT
#error "Macro index count not defined"
#endif

#ifndef MACROS_STORAGE
#error "Macro data storage not defined"
#endif

// Size in bytes of macro storage (including end offset)
#ifndef MACROS_SIZE
#error "Macro data storage size not defined"
#endif

// Size in bytes of program storage (including index)
#ifndef PROGRAM_STORAGE
#error "Program data storage not defined"
#endif

#ifndef PROGRAM_SIZE
#error "Program	data storage size not defined"
#endif

// Number of programs we support. We always permit concurrent program
// execution, so we limit the number of programs to the number of VMs.
#ifndef PROGRAM_COUNT
#error "Program interpreter count not defined"
#endif

#endif // __HARDWARE_H
