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

static const char* print_buffer;

void printing_set_buffer(const char* buf){
	print_buffer = buf;
}

uint8_t printing_buffer_empty(){
	return pgm_read_byte_near(print_buffer) == '\0';
}

void printing_Fill_KeyboardReport(KeyboardReport_Data_t* ReportData){
	// if the last report was a key, send empty. Otherwise send the
	// next character from print_buffer
	if(PrevKeyboardHIDReportBuffer.Modifier || PrevKeyboardHIDReportBuffer.KeyCode[0]){
		return; // empty report
	}
	else{
		char nextchar = pgm_read_byte_near(print_buffer++);
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

const char* const print_byte(const uint8_t byte){
	switch(byte){
	case 1:   { static const char msg[] PROGMEM = "01 "; return msg; }
	case 2:   { static const char msg[] PROGMEM = "02 "; return msg; }
	case 3:   { static const char msg[] PROGMEM = "03 "; return msg; }
	case 4:   { static const char msg[] PROGMEM = "04 "; return msg; }
	case 5:   { static const char msg[] PROGMEM = "05 "; return msg; }
	case 6:   { static const char msg[] PROGMEM = "06 "; return msg; }
	case 7:   { static const char msg[] PROGMEM = "07 "; return msg; }
	case 8:   { static const char msg[] PROGMEM = "08 "; return msg; }
	case 9:   { static const char msg[] PROGMEM = "09 "; return msg; }
	case 10:  { static const char msg[] PROGMEM = "0A "; return msg; }
	case 11:  { static const char msg[] PROGMEM = "0B "; return msg; }
	case 12:  { static const char msg[] PROGMEM = "0C "; return msg; }
	case 13:  { static const char msg[] PROGMEM = "0D "; return msg; }
	case 14:  { static const char msg[] PROGMEM = "0E "; return msg; }
	case 15:  { static const char msg[] PROGMEM = "0F "; return msg; }
	case 16:  { static const char msg[] PROGMEM = "10 "; return msg; }
	case 17:  { static const char msg[] PROGMEM = "11 "; return msg; }
	case 18:  { static const char msg[] PROGMEM = "12 "; return msg; }
	case 19:  { static const char msg[] PROGMEM = "13 "; return msg; }
	case 20:  { static const char msg[] PROGMEM = "14 "; return msg; }
	case 21:  { static const char msg[] PROGMEM = "15 "; return msg; }
	case 22:  { static const char msg[] PROGMEM = "16 "; return msg; }
	case 23:  { static const char msg[] PROGMEM = "17 "; return msg; }
	case 24:  { static const char msg[] PROGMEM = "18 "; return msg; }
	case 25:  { static const char msg[] PROGMEM = "19 "; return msg; }
	case 26:  { static const char msg[] PROGMEM = "1A "; return msg; }
	case 27:  { static const char msg[] PROGMEM = "1B "; return msg; }
	case 28:  { static const char msg[] PROGMEM = "1C "; return msg; }
	case 29:  { static const char msg[] PROGMEM = "1D "; return msg; }
	case 30:  { static const char msg[] PROGMEM = "1E "; return msg; }
	case 31:  { static const char msg[] PROGMEM = "1F "; return msg; }
	case 32:  { static const char msg[] PROGMEM = "20 "; return msg; }
	case 33:  { static const char msg[] PROGMEM = "21 "; return msg; }
	case 34:  { static const char msg[] PROGMEM = "22 "; return msg; }
	case 35:  { static const char msg[] PROGMEM = "23 "; return msg; }
	case 36:  { static const char msg[] PROGMEM = "24 "; return msg; }
	case 37:  { static const char msg[] PROGMEM = "25 "; return msg; }
	case 38:  { static const char msg[] PROGMEM = "26 "; return msg; }
	case 39:  { static const char msg[] PROGMEM = "27 "; return msg; }
	case 40:  { static const char msg[] PROGMEM = "28 "; return msg; }
	case 41:  { static const char msg[] PROGMEM = "29 "; return msg; }
	case 42:  { static const char msg[] PROGMEM = "2A "; return msg; }
	case 43:  { static const char msg[] PROGMEM = "2B "; return msg; }
	case 44:  { static const char msg[] PROGMEM = "2C "; return msg; }
	case 45:  { static const char msg[] PROGMEM = "2D "; return msg; }
	case 46:  { static const char msg[] PROGMEM = "2E "; return msg; }
	case 47:  { static const char msg[] PROGMEM = "2F "; return msg; }
	case 48:  { static const char msg[] PROGMEM = "30 "; return msg; }
	case 49:  { static const char msg[] PROGMEM = "31 "; return msg; }
	case 50:  { static const char msg[] PROGMEM = "32 "; return msg; }
	case 51:  { static const char msg[] PROGMEM = "33 "; return msg; }
	case 52:  { static const char msg[] PROGMEM = "34 "; return msg; }
	case 53:  { static const char msg[] PROGMEM = "35 "; return msg; }
	case 54:  { static const char msg[] PROGMEM = "36 "; return msg; }
	case 55:  { static const char msg[] PROGMEM = "37 "; return msg; }
	case 56:  { static const char msg[] PROGMEM = "38 "; return msg; }
	case 57:  { static const char msg[] PROGMEM = "39 "; return msg; }
	case 58:  { static const char msg[] PROGMEM = "3A "; return msg; }
	case 59:  { static const char msg[] PROGMEM = "3B "; return msg; }
	case 60:  { static const char msg[] PROGMEM = "3C "; return msg; }
	case 61:  { static const char msg[] PROGMEM = "3D "; return msg; }
	case 62:  { static const char msg[] PROGMEM = "3E "; return msg; }
	case 63:  { static const char msg[] PROGMEM = "3F "; return msg; }
	case 64:  { static const char msg[] PROGMEM = "40 "; return msg; }
	case 65:  { static const char msg[] PROGMEM = "41 "; return msg; }
	case 66:  { static const char msg[] PROGMEM = "42 "; return msg; }
	case 67:  { static const char msg[] PROGMEM = "43 "; return msg; }
	case 68:  { static const char msg[] PROGMEM = "44 "; return msg; }
	case 69:  { static const char msg[] PROGMEM = "45 "; return msg; }
	case 70:  { static const char msg[] PROGMEM = "46 "; return msg; }
	case 71:  { static const char msg[] PROGMEM = "47 "; return msg; }
	case 72:  { static const char msg[] PROGMEM = "48 "; return msg; }
	case 73:  { static const char msg[] PROGMEM = "49 "; return msg; }
	case 74:  { static const char msg[] PROGMEM = "4A "; return msg; }
	case 75:  { static const char msg[] PROGMEM = "4B "; return msg; }
	case 76:  { static const char msg[] PROGMEM = "4C "; return msg; }
	case 77:  { static const char msg[] PROGMEM = "4D "; return msg; }
	case 78:  { static const char msg[] PROGMEM = "4E "; return msg; }
	case 79:  { static const char msg[] PROGMEM = "4F "; return msg; }
	case 80:  { static const char msg[] PROGMEM = "50 "; return msg; }
	case 81:  { static const char msg[] PROGMEM = "51 "; return msg; }
	case 82:  { static const char msg[] PROGMEM = "52 "; return msg; }
	case 83:  { static const char msg[] PROGMEM = "53 "; return msg; }
	case 84:  { static const char msg[] PROGMEM = "54 "; return msg; }
	case 85:  { static const char msg[] PROGMEM = "55 "; return msg; }
	case 86:  { static const char msg[] PROGMEM = "56 "; return msg; }
	case 87:  { static const char msg[] PROGMEM = "57 "; return msg; }
	case 88:  { static const char msg[] PROGMEM = "58 "; return msg; }
	case 89:  { static const char msg[] PROGMEM = "59 "; return msg; }
	case 90:  { static const char msg[] PROGMEM = "5A "; return msg; }
	case 91:  { static const char msg[] PROGMEM = "5B "; return msg; }
	case 92:  { static const char msg[] PROGMEM = "5C "; return msg; }
	case 93:  { static const char msg[] PROGMEM = "5D "; return msg; }
	case 94:  { static const char msg[] PROGMEM = "5E "; return msg; }
	case 95:  { static const char msg[] PROGMEM = "5F "; return msg; }
	case 96:  { static const char msg[] PROGMEM = "60 "; return msg; }
	case 97:  { static const char msg[] PROGMEM = "61 "; return msg; }
	case 98:  { static const char msg[] PROGMEM = "62 "; return msg; }
	case 99:  { static const char msg[] PROGMEM = "63 "; return msg; }
	case 100: { static const char msg[] PROGMEM = "64 "; return msg; }
	case 101: { static const char msg[] PROGMEM = "65 "; return msg; }
	case 102: { static const char msg[] PROGMEM = "66 "; return msg; }
	case 103: { static const char msg[] PROGMEM = "67 "; return msg; }
	case 104: { static const char msg[] PROGMEM = "68 "; return msg; }
	case 105: { static const char msg[] PROGMEM = "69 "; return msg; }
	case 106: { static const char msg[] PROGMEM = "6A "; return msg; }
	case 107: { static const char msg[] PROGMEM = "6B "; return msg; }
	case 108: { static const char msg[] PROGMEM = "6C "; return msg; }
	case 109: { static const char msg[] PROGMEM = "6D "; return msg; }
	case 110: { static const char msg[] PROGMEM = "6E "; return msg; }
	case 111: { static const char msg[] PROGMEM = "6F "; return msg; }
	case 112: { static const char msg[] PROGMEM = "70 "; return msg; }
	case 113: { static const char msg[] PROGMEM = "71 "; return msg; }
	case 114: { static const char msg[] PROGMEM = "72 "; return msg; }
	case 115: { static const char msg[] PROGMEM = "73 "; return msg; }
	case 116: { static const char msg[] PROGMEM = "74 "; return msg; }
	case 117: { static const char msg[] PROGMEM = "75 "; return msg; }
	case 118: { static const char msg[] PROGMEM = "76 "; return msg; }
	case 119: { static const char msg[] PROGMEM = "77 "; return msg; }
	case 120: { static const char msg[] PROGMEM = "78 "; return msg; }
	case 121: { static const char msg[] PROGMEM = "79 "; return msg; }
	case 122: { static const char msg[] PROGMEM = "7A "; return msg; }
	case 123: { static const char msg[] PROGMEM = "7B "; return msg; }
	case 124: { static const char msg[] PROGMEM = "7C "; return msg; }
	case 125: { static const char msg[] PROGMEM = "7D "; return msg; }
	case 126: { static const char msg[] PROGMEM = "7E "; return msg; }
	case 127: { static const char msg[] PROGMEM = "7F "; return msg; }
	case 128: { static const char msg[] PROGMEM = "80 "; return msg; }
	case 129: { static const char msg[] PROGMEM = "81 "; return msg; }
	case 130: { static const char msg[] PROGMEM = "82 "; return msg; }
	case 131: { static const char msg[] PROGMEM = "83 "; return msg; }
	case 132: { static const char msg[] PROGMEM = "84 "; return msg; }
	case 133: { static const char msg[] PROGMEM = "85 "; return msg; }
	case 134: { static const char msg[] PROGMEM = "86 "; return msg; }
	case 135: { static const char msg[] PROGMEM = "87 "; return msg; }
	case 136: { static const char msg[] PROGMEM = "88 "; return msg; }
	case 137: { static const char msg[] PROGMEM = "89 "; return msg; }
	case 138: { static const char msg[] PROGMEM = "8A "; return msg; }
	case 139: { static const char msg[] PROGMEM = "8B "; return msg; }
	case 140: { static const char msg[] PROGMEM = "8C "; return msg; }
	case 141: { static const char msg[] PROGMEM = "8D "; return msg; }
	case 142: { static const char msg[] PROGMEM = "8E "; return msg; }
	case 143: { static const char msg[] PROGMEM = "8F "; return msg; }
	case 144: { static const char msg[] PROGMEM = "90 "; return msg; }
	case 145: { static const char msg[] PROGMEM = "91 "; return msg; }
	case 146: { static const char msg[] PROGMEM = "92 "; return msg; }
	case 147: { static const char msg[] PROGMEM = "93 "; return msg; }
	case 148: { static const char msg[] PROGMEM = "94 "; return msg; }
	case 149: { static const char msg[] PROGMEM = "95 "; return msg; }
	case 150: { static const char msg[] PROGMEM = "96 "; return msg; }
	case 151: { static const char msg[] PROGMEM = "97 "; return msg; }
	case 152: { static const char msg[] PROGMEM = "98 "; return msg; }
	case 153: { static const char msg[] PROGMEM = "99 "; return msg; }
	case 154: { static const char msg[] PROGMEM = "9A "; return msg; }
	case 155: { static const char msg[] PROGMEM = "9B "; return msg; }
	case 156: { static const char msg[] PROGMEM = "9C "; return msg; }
	case 157: { static const char msg[] PROGMEM = "9D "; return msg; }
	case 158: { static const char msg[] PROGMEM = "9E "; return msg; }
	case 159: { static const char msg[] PROGMEM = "9F "; return msg; }
	case 160: { static const char msg[] PROGMEM = "A0 "; return msg; }
	case 161: { static const char msg[] PROGMEM = "A1 "; return msg; }
	case 162: { static const char msg[] PROGMEM = "A2 "; return msg; }
	case 163: { static const char msg[] PROGMEM = "A3 "; return msg; }
	case 164: { static const char msg[] PROGMEM = "A4 "; return msg; }
	case 165: { static const char msg[] PROGMEM = "A5 "; return msg; }
	case 166: { static const char msg[] PROGMEM = "A6 "; return msg; }
	case 167: { static const char msg[] PROGMEM = "A7 "; return msg; }
	case 168: { static const char msg[] PROGMEM = "A8 "; return msg; }
	case 169: { static const char msg[] PROGMEM = "A9 "; return msg; }
	case 170: { static const char msg[] PROGMEM = "AA "; return msg; }
	case 171: { static const char msg[] PROGMEM = "AB "; return msg; }
	case 172: { static const char msg[] PROGMEM = "AC "; return msg; }
	case 173: { static const char msg[] PROGMEM = "AD "; return msg; }
	case 174: { static const char msg[] PROGMEM = "AE "; return msg; }
	case 175: { static const char msg[] PROGMEM = "AF "; return msg; }
	case 176: { static const char msg[] PROGMEM = "B0 "; return msg; }
	case 177: { static const char msg[] PROGMEM = "B1 "; return msg; }
	case 178: { static const char msg[] PROGMEM = "B2 "; return msg; }
	case 179: { static const char msg[] PROGMEM = "B3 "; return msg; }
	case 180: { static const char msg[] PROGMEM = "B4 "; return msg; }
	case 181: { static const char msg[] PROGMEM = "B5 "; return msg; }
	case 182: { static const char msg[] PROGMEM = "B6 "; return msg; }
	case 183: { static const char msg[] PROGMEM = "B7 "; return msg; }
	case 184: { static const char msg[] PROGMEM = "B8 "; return msg; }
	case 185: { static const char msg[] PROGMEM = "B9 "; return msg; }
	case 186: { static const char msg[] PROGMEM = "BA "; return msg; }
	case 187: { static const char msg[] PROGMEM = "BB "; return msg; }
	case 188: { static const char msg[] PROGMEM = "BC "; return msg; }
	case 189: { static const char msg[] PROGMEM = "BD "; return msg; }
	case 190: { static const char msg[] PROGMEM = "BE "; return msg; }
	case 191: { static const char msg[] PROGMEM = "BF "; return msg; }
	case 192: { static const char msg[] PROGMEM = "C0 "; return msg; }
	case 193: { static const char msg[] PROGMEM = "C1 "; return msg; }
	case 194: { static const char msg[] PROGMEM = "C2 "; return msg; }
	case 195: { static const char msg[] PROGMEM = "C3 "; return msg; }
	case 196: { static const char msg[] PROGMEM = "C4 "; return msg; }
	case 197: { static const char msg[] PROGMEM = "C5 "; return msg; }
	case 198: { static const char msg[] PROGMEM = "C6 "; return msg; }
	case 199: { static const char msg[] PROGMEM = "C7 "; return msg; }
	case 200: { static const char msg[] PROGMEM = "C8 "; return msg; }
	case 201: { static const char msg[] PROGMEM = "C9 "; return msg; }
	case 202: { static const char msg[] PROGMEM = "CA "; return msg; }
	case 203: { static const char msg[] PROGMEM = "CB "; return msg; }
	case 204: { static const char msg[] PROGMEM = "CC "; return msg; }
	case 205: { static const char msg[] PROGMEM = "CD "; return msg; }
	case 206: { static const char msg[] PROGMEM = "CE "; return msg; }
	case 207: { static const char msg[] PROGMEM = "CF "; return msg; }
	case 208: { static const char msg[] PROGMEM = "D0 "; return msg; }
	case 209: { static const char msg[] PROGMEM = "D1 "; return msg; }
	case 210: { static const char msg[] PROGMEM = "D2 "; return msg; }
	case 211: { static const char msg[] PROGMEM = "D3 "; return msg; }
	case 212: { static const char msg[] PROGMEM = "D4 "; return msg; }
	case 213: { static const char msg[] PROGMEM = "D5 "; return msg; }
	case 214: { static const char msg[] PROGMEM = "D6 "; return msg; }
	case 215: { static const char msg[] PROGMEM = "D7 "; return msg; }
	case 216: { static const char msg[] PROGMEM = "D8 "; return msg; }
	case 217: { static const char msg[] PROGMEM = "D9 "; return msg; }
	case 218: { static const char msg[] PROGMEM = "DA "; return msg; }
	case 219: { static const char msg[] PROGMEM = "DB "; return msg; }
	case 220: { static const char msg[] PROGMEM = "DC "; return msg; }
	case 221: { static const char msg[] PROGMEM = "DD "; return msg; }
	case 222: { static const char msg[] PROGMEM = "DE "; return msg; }
	case 223: { static const char msg[] PROGMEM = "DF "; return msg; }
	case 224: { static const char msg[] PROGMEM = "E0 "; return msg; }
	case 225: { static const char msg[] PROGMEM = "E1 "; return msg; }
	case 226: { static const char msg[] PROGMEM = "E2 "; return msg; }
	case 227: { static const char msg[] PROGMEM = "E3 "; return msg; }
	case 228: { static const char msg[] PROGMEM = "E4 "; return msg; }
	case 229: { static const char msg[] PROGMEM = "E5 "; return msg; }
	case 230: { static const char msg[] PROGMEM = "E6 "; return msg; }
	case 231: { static const char msg[] PROGMEM = "E7 "; return msg; }
	case 232: { static const char msg[] PROGMEM = "E8 "; return msg; }
	case 233: { static const char msg[] PROGMEM = "E9 "; return msg; }
	case 234: { static const char msg[] PROGMEM = "EA "; return msg; }
	case 235: { static const char msg[] PROGMEM = "EB "; return msg; }
	case 236: { static const char msg[] PROGMEM = "EC "; return msg; }
	case 237: { static const char msg[] PROGMEM = "ED "; return msg; }
	case 238: { static const char msg[] PROGMEM = "EE "; return msg; }
	case 239: { static const char msg[] PROGMEM = "EF "; return msg; }
	case 240: { static const char msg[] PROGMEM = "F0 "; return msg; }
	case 241: { static const char msg[] PROGMEM = "F1 "; return msg; }
	case 242: { static const char msg[] PROGMEM = "F2 "; return msg; }
	case 243: { static const char msg[] PROGMEM = "F3 "; return msg; }
	case 244: { static const char msg[] PROGMEM = "F4 "; return msg; }
	case 245: { static const char msg[] PROGMEM = "F5 "; return msg; }
	case 246: { static const char msg[] PROGMEM = "F6 "; return msg; }
	case 247: { static const char msg[] PROGMEM = "F7 "; return msg; }
	case 248: { static const char msg[] PROGMEM = "F8 "; return msg; }
	case 249: { static const char msg[] PROGMEM = "F9 "; return msg; }
	case 250: { static const char msg[] PROGMEM = "FA "; return msg; }
	case 251: { static const char msg[] PROGMEM = "FB "; return msg; }
	case 252: { static const char msg[] PROGMEM = "FC "; return msg; }
	case 253: { static const char msg[] PROGMEM = "FD "; return msg; }
	case 254: { static const char msg[] PROGMEM = "FE "; return msg; }
	case 255:
	default:
		{ static const char msg[] PROGMEM = "FF "; return msg; }
	}
}
