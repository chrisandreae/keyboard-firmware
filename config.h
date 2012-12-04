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

#ifndef __CONFIG_H
#define __CONFIG_H

#include "hardware.h"
#include "keystate.h"

// Number of programs we support. We always permit concurrent program
// execution, so we limit the number of programs to the number of VMs.
#define NUM_PROGRAMS 6

// size in bytes of program storage (including index)
#define PROGRAMS_SIZE 1024

// Configuration is saved in the eeprom
typedef struct _configuration_flags {
	unsigned char key_sound_enabled:1;
	unsigned char packing:7;
} configuration_flags;

// returns eeprom address of logical_to_hid_map
hid_keycode* config_get_mapping(void);

hid_keycode config_get_definition(logical_keycode l_key);
void config_save_definition(logical_keycode l_key, hid_keycode h_key);

void config_init(void);
void config_reset_defaults(void);
void config_reset_fully(void);
uint8_t config_delete_layout(uint8_t num);
uint8_t config_save_layout(uint8_t num);
uint8_t config_load_layout(uint8_t num);

configuration_flags config_get_flags(void);
void config_save_flags(configuration_flags state);

uint8_t* config_get_programs();

struct _program;
const struct _program* config_get_program(uint8_t idx);
void config_reset_program_defaults();

#endif // __CONFIG_H
