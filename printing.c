/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  This file is offered under either of the GNU GPL v2 or MIT licences
  below in order that it may be used with either of the V-USB or LUFA
  USB libraries.

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

#include "printing.h"
#include "Keyboard.h"
#include "keystate.h"

static buf_type print_buffer_type;
static const char* print_buffer;

void printing_set_buffer(const char* buf, buf_type typ){
	print_buffer = buf;
	print_buffer_type = typ;
}

uint8_t print_buffer_get(void){
	switch(print_buffer_type){
	case BUF_MEM:
		return *print_buffer;
	case BUF_PGM:
		return pgm_read_byte_near(print_buffer);
	case BUF_EE:
	case BUF_EEEXT:
	default:
		return 0; // unsupported;
	}
}

uint8_t printing_buffer_empty(void){
	return print_buffer_get() == '\0';
}

void printing_Fill_KeyboardReport(KeyboardReport_Data_t* ReportData){
	// if the last report was a key, send empty. Otherwise send the
	// next character from print_buffer
	if(PrevKeyboardHIDReportBuffer.Modifier || PrevKeyboardHIDReportBuffer.KeyCode[0]){
		return; // empty report
	}
	else{
		char nextchar = print_buffer_get();
		print_buffer++;
		uint8_t key, mod;
		char_to_keys(nextchar, &key, &mod);
		ReportData->Modifier = mod;
		ReportData->KeyCode[0] = key;
	}
}

void char_to_keys(const char nextchar, hid_keycode* nextkey, hid_keycode* nextmod){
	*nextkey = 0;
	*nextmod = 0;

	// letters:
	uint8_t l = nextchar | 0x20;
	if('a' <= l && 'z' >= l){
		*nextkey = HID_KEYBOARD_SC_A + (l - 'a');
		if(!(nextchar & 0x20)) *nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
	}
	else{
		switch(nextchar){
		case ' ':
			*nextkey = HID_KEYBOARD_SC_SPACE;
			break;
		case '!':
			*nextkey = HID_KEYBOARD_SC_1_AND_EXCLAMATION;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '"':
			*nextkey = HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '#':
			*nextkey = HID_KEYBOARD_SC_3_AND_HASHMARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '$':
			*nextkey = HID_KEYBOARD_SC_4_AND_DOLLAR;
			*nextmod =  HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '%':
			*nextkey = HID_KEYBOARD_SC_5_AND_PERCENTAGE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '&':
			*nextkey = HID_KEYBOARD_SC_7_AND_AND_AMPERSAND;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '\'':
			*nextkey = HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE;
			break;
		case '(':
			*nextkey = HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case ')':
			*nextkey = HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '*':
			*nextkey = HID_KEYBOARD_SC_8_AND_ASTERISK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '+':
			*nextkey = HID_KEYBOARD_SC_EQUAL_AND_PLUS;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case ',':
			*nextkey = HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN;
			break;
		case '-':
			*nextkey = HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE;
			break;
		case '.':
			*nextkey = HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN;
			break;
		case '/':
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			break;
		case '0':
			*nextkey = HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS;
			break;
		case '1':
			*nextkey = HID_KEYBOARD_SC_1_AND_EXCLAMATION;
			break;
		case '2':
			*nextkey = HID_KEYBOARD_SC_2_AND_AT;
			break;
		case '3':
			*nextkey = HID_KEYBOARD_SC_3_AND_HASHMARK;
			break;
		case '4':
			*nextkey = HID_KEYBOARD_SC_4_AND_DOLLAR;
			break;
		case '5':
			*nextkey = HID_KEYBOARD_SC_5_AND_PERCENTAGE;
			break;
		case '6':
			*nextkey = HID_KEYBOARD_SC_6_AND_CARET;
			break;
		case '7':
			*nextkey = HID_KEYBOARD_SC_7_AND_AND_AMPERSAND;
			break;
		case '8':
			*nextkey = HID_KEYBOARD_SC_8_AND_ASTERISK;
			break;
		case '9':
			*nextkey = HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS;
			break;
		case ':':
			*nextkey = HID_KEYBOARD_SC_SEMICOLON_AND_COLON;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case ';':
			*nextkey = HID_KEYBOARD_SC_SEMICOLON_AND_COLON;
			break;
		case '<':
			*nextkey = HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '=':
			*nextkey = HID_KEYBOARD_SC_EQUAL_AND_PLUS;
			break;
		case '>':
			*nextkey = HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '?':
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '@':
			*nextkey = HID_KEYBOARD_SC_2_AND_AT;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '[':
			*nextkey = HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE;
			break;
		case '\\':
			*nextkey = HID_KEYBOARD_SC_BACKSLASH_AND_PIPE;
			break;
		case ']':
			*nextkey = HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE;
			break;
		case '^':
			*nextkey = HID_KEYBOARD_SC_6_AND_CARET;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '_':
			*nextkey = HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '`':
			*nextkey = HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '{':
			*nextkey = HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '|':
			*nextkey = HID_KEYBOARD_SC_BACKSLASH_AND_PIPE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '}':
			*nextkey = HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		case '~':
			*nextkey = HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE;
			break;
		case '\n':
			*nextkey = HID_KEYBOARD_SC_ENTER;
			break;
		case '\t':
			*nextkey = HID_KEYBOARD_SC_TAB;
			break;
		default:
			*nextkey = HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK;
			*nextmod = HID_KEYBOARD_MODIFER_LEFTSHIFT;
			break;
		}
	}
}

// For serial eeprom testing, to allow the eeprom contents to be dumped
const char* byte_to_str(uint8_t byte){
	static char buf[4] = {'x', 'x', ' ', '\0'};
	for(int8_t i = 1; i >= 0; --i){
		uint8_t n = byte & 0xF;
		buf[i] = n >= 0xA ? ('A' + n - 0xA) : ('0' + n);
		byte >>= 4;
	}
	return buf;
}
