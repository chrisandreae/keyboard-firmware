/* Kinesis Matrix */

// The Kinesis has few physical keys, so has a "keypad layer" toggle
// which has a separate binding for each key. This provides access not
// only to the numpad, but to the windows and menu keys.
#define KEYPAD_LAYER 1

// if the keypad mode is selected, and a logical key greater than KEYPAD_LAYER_START
// is read, add KEYPAD_LAYER_SIZE to look up the mapping
#define KEYPAD_LAYER_START 2
#define KEYPAD_LAYER_SIZE  84

#define NUM_LOGICAL_KEYS (2 + (KEYPAD_LAYER_SIZE * 2)) //86 physical keys, 84 of which (all but keypad/program) have a separate keypad layer mapping

#define MATRIX_COLS 10 // 8 demultiplexer selected matrix columns, and two direct button inputs (pins 5 and 6)
#define MATRIX_ROWS 16 // 2 74LS138 1-of-8 demultiplexers

// Logical keys we have
enum logical_keys {
	LOGICAL_KEY_KEYPAD, // KY
	LOGICAL_KEY_PROGRAM, // PG
	// non-keypad layer
	LOGICAL_KEY_A,
	LOGICAL_KEY_B,
	LOGICAL_KEY_C,
	LOGICAL_KEY_D,
	LOGICAL_KEY_E,
	LOGICAL_KEY_F,
	LOGICAL_KEY_G,
	LOGICAL_KEY_H,
	LOGICAL_KEY_I,
	LOGICAL_KEY_J,
	LOGICAL_KEY_K,
	LOGICAL_KEY_L,
	LOGICAL_KEY_M,
	LOGICAL_KEY_N,
	LOGICAL_KEY_O,
	LOGICAL_KEY_P,
	LOGICAL_KEY_Q,
	LOGICAL_KEY_R,
	LOGICAL_KEY_S,
	LOGICAL_KEY_T,
	LOGICAL_KEY_U,
	LOGICAL_KEY_V,
	LOGICAL_KEY_W,
	LOGICAL_KEY_X,
	LOGICAL_KEY_Y,
	LOGICAL_KEY_Z,
	LOGICAL_KEY_1,
	LOGICAL_KEY_2,
	LOGICAL_KEY_3,
	LOGICAL_KEY_4,
	LOGICAL_KEY_5,
	LOGICAL_KEY_6,
	LOGICAL_KEY_7,
	LOGICAL_KEY_8,
	LOGICAL_KEY_9,
	LOGICAL_KEY_0,
	LOGICAL_KEY_F1,
	LOGICAL_KEY_F2,
	LOGICAL_KEY_F3,
	LOGICAL_KEY_F4,
	LOGICAL_KEY_F5,
	LOGICAL_KEY_F6,
	LOGICAL_KEY_F7,
	LOGICAL_KEY_F8,
	LOGICAL_KEY_F9,
	LOGICAL_KEY_F10,
	LOGICAL_KEY_F11,
	LOGICAL_KEY_F12,
	LOGICAL_KEY_EQUALS,
	LOGICAL_KEY_TAB,
	LOGICAL_KEY_ESC,
	LOGICAL_KEY_CAPSLOCK,
	LOGICAL_KEY_LSHIFT,
	LOGICAL_KEY_UP,
	LOGICAL_KEY_DOWN,
	LOGICAL_KEY_LEFT,
	LOGICAL_KEY_RIGHT,
	LOGICAL_KEY_LSQUARE,
	LOGICAL_KEY_RSQUARE,
	LOGICAL_KEY_SCROLLLOCK,
	LOGICAL_KEY_PRINTSCREEN,
	LOGICAL_KEY_INTERNATIONAL, // insert in keypad layer
	LOGICAL_KEY_BACKTICK,
	LOGICAL_KEY_HYPHEN,
	LOGICAL_KEY_PAUSE,
	LOGICAL_KEY_END,
	LOGICAL_KEY_HOME,
	LOGICAL_KEY_DELETE,
	LOGICAL_KEY_BACKSPACE,
	LOGICAL_KEY_L_ALT,
	LOGICAL_KEY_L_CTRL,
	LOGICAL_KEY_BACKSLASH,
	LOGICAL_KEY_PGDN,
	LOGICAL_KEY_PGUP,
	LOGICAL_KEY_ENTER,
	LOGICAL_KEY_SPACE,
	LOGICAL_KEY_R_ALT,
	LOGICAL_KEY_R_CTRL,
	LOGICAL_KEY_SEMICOLON,
	LOGICAL_KEY_QUOTE,
	LOGICAL_KEY_COMMA,
	LOGICAL_KEY_PERIOD,
	LOGICAL_KEY_SLASH,
	LOGICAL_KEY_RSHIFT,
	// The keypad layer duplicates the previous 84 keys
};

#define KEY_NONE NO_KEY
// because the matrix is not tightly packed, we want a map from matrix
// position to logical key.
const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM = {
    /*      PIN:       5                        6                   21                  22               23                     24                     25                       26                     27                   28      */
    /*U4-Y0*/   { LOGICAL_KEY_KEYPAD,  LOGICAL_KEY_PROGRAM,  LOGICAL_KEY_F6,     LOGICAL_KEY_F3,  LOGICAL_KEY_ESC,      LOGICAL_KEY_F9,      LOGICAL_KEY_F12,           LOGICAL_KEY_PAUSE,     KEY_NONE,              KEY_NONE               }, // 0 0 0 0
    /*U4-Y1*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_F8,     LOGICAL_KEY_F5,  LOGICAL_KEY_F2,       LOGICAL_KEY_F11,     LOGICAL_KEY_SCROLLLOCK,    KEY_NONE,              KEY_NONE,              KEY_NONE               }, // 0 0 0 1
    /*U4-Y2*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_F7,     LOGICAL_KEY_F4,  LOGICAL_KEY_F1,       LOGICAL_KEY_F10,     LOGICAL_KEY_PRINTSCREEN,   KEY_NONE,              KEY_NONE,              KEY_NONE               }, // 0 0 1 0
    /*U4-Y3*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_5,      LOGICAL_KEY_T,   LOGICAL_KEY_G,        LOGICAL_KEY_B,       LOGICAL_KEY_RIGHT,         LOGICAL_KEY_L_ALT,     LOGICAL_KEY_L_CTRL,    KEY_NONE               }, // 0 0 1 1
    /*U4-Y4*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_4,      LOGICAL_KEY_R,   LOGICAL_KEY_F,        LOGICAL_KEY_V,       KEY_NONE,                  KEY_NONE,              LOGICAL_KEY_DELETE,    LOGICAL_KEY_R_ALT      }, // 0 1 0 0
    /*U4-Y5*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_3,      LOGICAL_KEY_E,   LOGICAL_KEY_D,        LOGICAL_KEY_C,       LOGICAL_KEY_LEFT,          LOGICAL_KEY_HOME,      LOGICAL_KEY_BACKSPACE, KEY_NONE               }, // 0 1 0 1
    /*U4-Y6*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_2,      LOGICAL_KEY_W,   LOGICAL_KEY_S,        LOGICAL_KEY_X,       LOGICAL_KEY_INTERNATIONAL, KEY_NONE,              LOGICAL_KEY_R_CTRL,    LOGICAL_KEY_PGUP       }, // 0 1 1 0
    /*U4-Y7*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_1,      LOGICAL_KEY_Q,   LOGICAL_KEY_A,        LOGICAL_KEY_Z,       LOGICAL_KEY_BACKTICK,      LOGICAL_KEY_END,       LOGICAL_KEY_ENTER,     KEY_NONE               }, // 0 1 1 1
    /*U5-Y0*/   { KEY_NONE,            KEY_NONE,             LOGICAL_KEY_EQUALS, LOGICAL_KEY_TAB, LOGICAL_KEY_CAPSLOCK, LOGICAL_KEY_LSHIFT,  KEY_NONE,                  KEY_NONE,              LOGICAL_KEY_SPACE,     LOGICAL_KEY_PGDN       }, // 1 0 0 0
    /*U5-Y1*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_UP,      LOGICAL_KEY_6,             LOGICAL_KEY_Y,         LOGICAL_KEY_H,         LOGICAL_KEY_N          }, // 1 0 0 1
    /*U5-Y2*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             KEY_NONE,            LOGICAL_KEY_7,             LOGICAL_KEY_U,         LOGICAL_KEY_J,         LOGICAL_KEY_M          }, // 1 0 1 0
    /*U5-Y3*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_DOWN,    LOGICAL_KEY_8,             LOGICAL_KEY_I,         LOGICAL_KEY_K,         LOGICAL_KEY_COMMA      }, // 1 0 1 1
    /*U5-Y4*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             KEY_NONE,            KEY_NONE,                  KEY_NONE,              KEY_NONE,              KEY_NONE               }, // 1 1 0 0
    /*U5-Y5*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_LSQUARE, LOGICAL_KEY_9,             LOGICAL_KEY_O,         LOGICAL_KEY_L,         LOGICAL_KEY_PERIOD     }, // 1 1 0 1
    /*U5-Y6*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             LOGICAL_KEY_RSQUARE, LOGICAL_KEY_0,             LOGICAL_KEY_P,         LOGICAL_KEY_SEMICOLON, LOGICAL_KEY_SLASH      }, // 1 1 1 0
    /*U5-Y7*/   { KEY_NONE,            KEY_NONE,             KEY_NONE,           KEY_NONE,        KEY_NONE,             KEY_NONE,            LOGICAL_KEY_HYPHEN,        LOGICAL_KEY_BACKSLASH, LOGICAL_KEY_QUOTE,     LOGICAL_KEY_RSHIFT     }  // 1 1 1 1
};  

#undef KEY_NONE

/* For each key, maps an index position to a default HID key code. */
/* stored in flash. */
const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM = {
	SPECIAL_HID_KEY_KEYPAD, 						   //	LOGICAL_KEY_KEYPAD
	SPECIAL_HID_KEY_PROGRAM, 						   //	LOGICAL_KEY_PROGRAM
	// non-keypad layer
	HID_KEYBOARD_SC_A, 								   //	LOGICAL_KEY_A
	HID_KEYBOARD_SC_B, 								   //	LOGICAL_KEY_B
	HID_KEYBOARD_SC_C, 								   //	LOGICAL_KEY_C
	HID_KEYBOARD_SC_D, 								   //	LOGICAL_KEY_D
	HID_KEYBOARD_SC_E, 								   //	LOGICAL_KEY_E
	HID_KEYBOARD_SC_F, 								   //	LOGICAL_KEY_F
	HID_KEYBOARD_SC_G, 								   //	LOGICAL_KEY_G
	HID_KEYBOARD_SC_H, 								   //	LOGICAL_KEY_H
	HID_KEYBOARD_SC_I, 								   //	LOGICAL_KEY_I
	HID_KEYBOARD_SC_J, 								   //	LOGICAL_KEY_J
	HID_KEYBOARD_SC_K, 								   //	LOGICAL_KEY_K
	HID_KEYBOARD_SC_L, 								   //	LOGICAL_KEY_L
	HID_KEYBOARD_SC_M, 								   //	LOGICAL_KEY_M
	HID_KEYBOARD_SC_N, 								   //	LOGICAL_KEY_N
	HID_KEYBOARD_SC_O, 								   //	LOGICAL_KEY_O
	HID_KEYBOARD_SC_P, 								   //	LOGICAL_KEY_P
	HID_KEYBOARD_SC_Q, 								   //	LOGICAL_KEY_Q
	HID_KEYBOARD_SC_R, 								   //	LOGICAL_KEY_R
	HID_KEYBOARD_SC_S, 								   //	LOGICAL_KEY_S
	HID_KEYBOARD_SC_T, 								   //	LOGICAL_KEY_T
	HID_KEYBOARD_SC_U, 								   //	LOGICAL_KEY_U
	HID_KEYBOARD_SC_V, 								   //	LOGICAL_KEY_V
	HID_KEYBOARD_SC_W, 								   //	LOGICAL_KEY_W
	HID_KEYBOARD_SC_X, 								   //	LOGICAL_KEY_X
	HID_KEYBOARD_SC_Y, 								   //	LOGICAL_KEY_Y
	HID_KEYBOARD_SC_Z, 								   //	LOGICAL_KEY_Z
	HID_KEYBOARD_SC_1_AND_EXCLAMATION, 				   //	LOGICAL_KEY_1
	HID_KEYBOARD_SC_2_AND_AT, 						   //	LOGICAL_KEY_2
	HID_KEYBOARD_SC_3_AND_HASHMARK, 				   //	LOGICAL_KEY_3
	HID_KEYBOARD_SC_4_AND_DOLLAR, 					   //	LOGICAL_KEY_4
	HID_KEYBOARD_SC_5_AND_PERCENTAGE, 				   //	LOGICAL_KEY_5
	HID_KEYBOARD_SC_6_AND_CARET, 					   //	LOGICAL_KEY_6
	HID_KEYBOARD_SC_7_AND_AND_AMPERSAND, 			   //	LOGICAL_KEY_7
	HID_KEYBOARD_SC_8_AND_ASTERISK, 				   //	LOGICAL_KEY_8
	HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS, 		   //	LOGICAL_KEY_9
	HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS, 		   //	LOGICAL_KEY_0
	HID_KEYBOARD_SC_F1, 							   //	LOGICAL_KEY_F1
	HID_KEYBOARD_SC_F2, 							   //	LOGICAL_KEY_F2
	HID_KEYBOARD_SC_F3, 							   //	LOGICAL_KEY_F3
	HID_KEYBOARD_SC_F4, 							   //	LOGICAL_KEY_F4
	HID_KEYBOARD_SC_F5, 							   //	LOGICAL_KEY_F5
	HID_KEYBOARD_SC_F6, 							   //	LOGICAL_KEY_F6
	HID_KEYBOARD_SC_F7, 							   //	LOGICAL_KEY_F7
	HID_KEYBOARD_SC_F8, 							   //	LOGICAL_KEY_F8
	HID_KEYBOARD_SC_F9, 							   //	LOGICAL_KEY_F9
	HID_KEYBOARD_SC_F10, 							   //	LOGICAL_KEY_F10
	HID_KEYBOARD_SC_F11, 							   //	LOGICAL_KEY_F11
	HID_KEYBOARD_SC_F12, 							   //	LOGICAL_KEY_F12
	HID_KEYBOARD_SC_EQUAL_AND_PLUS, 				   //	LOGICAL_KEY_EQUALS
	HID_KEYBOARD_SC_TAB, 							   //	LOGICAL_KEY_TAB
	HID_KEYBOARD_SC_ESCAPE, 						   //	LOGICAL_KEY_ESC
	HID_KEYBOARD_SC_CAPS_LOCK, 						   //	LOGICAL_KEY_CAPSLOCK
	HID_KEYBOARD_SC_LEFT_SHIFT, 					   //	LOGICAL_KEY_LSHIFT
	HID_KEYBOARD_SC_UP_ARROW, 						   //	LOGICAL_KEY_UP
	HID_KEYBOARD_SC_DOWN_ARROW, 					   //	LOGICAL_KEY_DOWN
	HID_KEYBOARD_SC_LEFT_ARROW, 					   //	LOGICAL_KEY_LEFT
	HID_KEYBOARD_SC_RIGHT_ARROW, 					   //	LOGICAL_KEY_RIGHT
	HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE, //	LOGICAL_KEY_LSQUARE
	HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE, //	LOGICAL_KEY_RSQUARE
	HID_KEYBOARD_SC_SCROLL_LOCK, 					   //	LOGICAL_KEY_SCROLLLOCK
	HID_KEYBOARD_SC_PRINT_SCREEN, 					   //	LOGICAL_KEY_PRINTSCREEN
	HID_KEYBOARD_SC_NON_US_BACKSLASH_AND_PIPE, 		   //	LOGICAL_KEY_INTERNATIONAL
	HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE, 		   //	LOGICAL_KEY_BACKTICK
	HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE, 			   //	LOGICAL_KEY_HYPHEN
	HID_KEYBOARD_SC_PAUSE, 							   //	LOGICAL_KEY_PAUSE
	HID_KEYBOARD_SC_END, 					   		   //	LOGICAL_KEY_END
	HID_KEYBOARD_SC_HOME, 						   	   //	LOGICAL_KEY_HOME
	HID_KEYBOARD_SC_DELETE, 						   //	LOGICAL_KEY_DELETE
	HID_KEYBOARD_SC_BACKSPACE, 						   //	LOGICAL_KEY_BACKSPACE
	HID_KEYBOARD_SC_LEFT_ALT, 						   //	LOGICAL_KEY_L_ALT
	HID_KEYBOARD_SC_LEFT_CONTROL, 					   //	LOGICAL_KEY_L_CTRL
	HID_KEYBOARD_SC_BACKSLASH_AND_PIPE, 			   //	LOGICAL_KEY_BACKSLASH
	HID_KEYBOARD_SC_PAGE_DOWN, 					   	   //	LOGICAL_KEY_PGDN
	HID_KEYBOARD_SC_PAGE_UP, 						   //	LOGICAL_KEY_PGUP
	HID_KEYBOARD_SC_ENTER, 						   	   //	LOGICAL_KEY_ENTER
	HID_KEYBOARD_SC_SPACE, 						   	   //	LOGICAL_KEY_SPACE
	HID_KEYBOARD_SC_RIGHT_ALT, 						   //	LOGICAL_KEY_R_ALT
	HID_KEYBOARD_SC_RIGHT_CONTROL, 					   //	LOGICAL_KEY_R_CTRL
	HID_KEYBOARD_SC_SEMICOLON_AND_COLON, 			   //	LOGICAL_KEY_SEMICOLON
	HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE, 			   //	LOGICAL_KEY_QUOTE
	HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN, 		   //	LOGICAL_KEY_COMMA
	HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN, 		   //	LOGICAL_KEY_PERIOD
	HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK, 		   //	LOGICAL_KEY_SLASH
	HID_KEYBOARD_SC_RIGHT_SHIFT, 					   //	LOGICAL_KEY_RSHIFT
		/////////////////////////////////////////////////                              *** keypad mode default differs to base layer
	HID_KEYBOARD_SC_A, 								   //  LOGICAL_KEY_KP_A,
	HID_KEYBOARD_SC_B, 								   //  LOGICAL_KEY_KP_B,
	HID_KEYBOARD_SC_C, 								   //  LOGICAL_KEY_KP_C,
	SPECIAL_HID_KEY_MOUSE_BACK,						   //  LOGICAL_KEY_KP_D,           ***
	SPECIAL_HID_KEY_MOUSE_FWD, 						   //  LOGICAL_KEY_KP_E,           ***
	SPECIAL_HID_KEY_MOUSE_RIGHT,				       //  LOGICAL_KEY_KP_F,           ***
	HID_KEYBOARD_SC_G, 								   //  LOGICAL_KEY_KP_G,
	HID_KEYBOARD_SC_H, 								   //  LOGICAL_KEY_KP_H,
	HID_KEYBOARD_SC_KEYPAD_8_AND_UP_ARROW, 	   		   //  LOGICAL_KEY_KP_I, 		   ***
	HID_KEYBOARD_SC_KEYPAD_4_AND_LEFT_ARROW,   		   //  LOGICAL_KEY_KP_J, 		   ***
	HID_KEYBOARD_SC_KEYPAD_5, 			   			   //  LOGICAL_KEY_KP_K, 		   ***
	HID_KEYBOARD_SC_KEYPAD_6_AND_RIGHT_ARROW, 		   //  LOGICAL_KEY_KP_L,           ***
	HID_KEYBOARD_SC_KEYPAD_1_AND_END, 	   			   //  LOGICAL_KEY_KP_M, 		   ***
	HID_KEYBOARD_SC_N, 								   //  LOGICAL_KEY_KP_N,
	HID_KEYBOARD_SC_KEYPAD_9_AND_PAGE_UP, 	   		   //  LOGICAL_KEY_KP_O, 		   ***
	HID_KEYBOARD_SC_KEYPAD_MINUS, 				   	   //  LOGICAL_KEY_KP_P, 		   ***
	HID_KEYBOARD_SC_Q, 								   //  LOGICAL_KEY_KP_Q,
	HID_KEYBOARD_SC_R, 					               //  LOGICAL_KEY_KP_R,
	SPECIAL_HID_KEY_MOUSE_LEFT, 				       //  LOGICAL_KEY_KP_S,           ***
	HID_KEYBOARD_SC_T, 								   //  LOGICAL_KEY_KP_T,
	HID_KEYBOARD_SC_KEYPAD_7_AND_HOME, 		   		   //  LOGICAL_KEY_KP_U, 		   ***
	HID_KEYBOARD_SC_V, 								   //  LOGICAL_KEY_KP_V,
	HID_KEYBOARD_SC_W, 					               //  LOGICAL_KEY_KP_W,
	HID_KEYBOARD_SC_X, 								   //  LOGICAL_KEY_KP_X,
	HID_KEYBOARD_SC_Y, 								   //  LOGICAL_KEY_KP_Y,
	HID_KEYBOARD_SC_Z, 								   //  LOGICAL_KEY_KP_Z,
	HID_KEYBOARD_SC_1_AND_EXCLAMATION, 				   //  LOGICAL_KEY_KP_1,
	HID_KEYBOARD_SC_2_AND_AT, 						   //  LOGICAL_KEY_KP_2,
	HID_KEYBOARD_SC_3_AND_HASHMARK, 				   //  LOGICAL_KEY_KP_3,
	HID_KEYBOARD_SC_4_AND_DOLLAR, 					   //  LOGICAL_KEY_KP_4,
	HID_KEYBOARD_SC_5_AND_PERCENTAGE, 				   //  LOGICAL_KEY_KP_5,
	HID_KEYBOARD_SC_6_AND_CARET, 					   //  LOGICAL_KEY_KP_6,
	HID_KEYBOARD_SC_NUM_LOCK,     			           //  LOGICAL_KEY_KP_7, 		   ***
	HID_KEYBOARD_SC_EQUAL_AND_PLUS, 		           //  LOGICAL_KEY_KP_8, 		   *** // _SC_KEYPAD_EQUAL_SIGN (0x86) is ignored by windows, other docs say kp= is 0x67
	HID_KEYBOARD_SC_KEYPAD_SLASH, 		               //  LOGICAL_KEY_KP_9, 		   ***
	HID_KEYBOARD_SC_KEYPAD_ASTERISK, 		           //  LOGICAL_KEY_KP_0, 		   ***
	HID_KEYBOARD_SC_F1, 							   //  LOGICAL_KEY_KP_F1,
	HID_KEYBOARD_SC_F2, 							   //  LOGICAL_KEY_KP_F2,
	HID_KEYBOARD_SC_F3, 							   //  LOGICAL_KEY_KP_F3,
	HID_KEYBOARD_SC_F4, 							   //  LOGICAL_KEY_KP_F4,
	HID_KEYBOARD_SC_F5, 							   //  LOGICAL_KEY_KP_F5,
	HID_KEYBOARD_SC_F6, 							   //  LOGICAL_KEY_KP_F6,
	HID_KEYBOARD_SC_F7, 							   //  LOGICAL_KEY_KP_F7,
	HID_KEYBOARD_SC_F8, 							   //  LOGICAL_KEY_KP_F8,
	HID_KEYBOARD_SC_F9, 							   //  LOGICAL_KEY_KP_F9,
	HID_KEYBOARD_SC_F10, 							   //  LOGICAL_KEY_KP_F10,
	HID_KEYBOARD_SC_F11, 							   //  LOGICAL_KEY_KP_F11,
	HID_KEYBOARD_SC_F12, 							   //  LOGICAL_KEY_KP_F12,
	HID_KEYBOARD_SC_EQUAL_AND_PLUS, 				   //  LOGICAL_KEY_KP_EQUALS,
	HID_KEYBOARD_SC_TAB, 							   //  LOGICAL_KEY_KP_TAB,
	HID_KEYBOARD_SC_ESCAPE, 						   //  LOGICAL_KEY_KP_ESC,
	NO_KEY, 						   				   //  LOGICAL_KEY_KP_CAPSLOCK,    ***
	HID_KEYBOARD_SC_LEFT_SHIFT, 					   //  LOGICAL_KEY_KP_LSHIFT,
	HID_KEYBOARD_SC_UP_ARROW, 						   //  LOGICAL_KEY_KP_UP,
	HID_KEYBOARD_SC_DOWN_ARROW, 					   //  LOGICAL_KEY_KP_DOWN,
	HID_KEYBOARD_SC_LEFT_ARROW, 					   //  LOGICAL_KEY_KP_LEFT,
	HID_KEYBOARD_SC_RIGHT_ARROW, 					   //  LOGICAL_KEY_KP_RIGHT,
	HID_KEYBOARD_SC_KEYPAD_DOT_AND_DELETE, 			   //  LOGICAL_KEY_KP_LSQUARE, 	   ***
	HID_KEYBOARD_SC_KEYPAD_ENTER, 					   //  LOGICAL_KEY_KP_RSQUARE, 	   ***
	HID_KEYBOARD_SC_RIGHT_GUI, 					   	   //  LOGICAL_KEY_KP_SCROLLLOCK,  ***
	HID_KEYBOARD_SC_LEFT_GUI, 					   	   //  LOGICAL_KEY_KP_PRINTSCREEN, ***
	HID_KEYBOARD_SC_INSERT, 						   //  LOGICAL_KEY_KP_INTERNATIONAL, ***
	HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE, 		   //  LOGICAL_KEY_KP_BACKTICK,
	HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE, 			   //  LOGICAL_KEY_KP_HYPHEN,
	0x65, 							                   //  LOGICAL_KEY_KP_PAUSE, 	   ***   // this seems to be the windows context menu key. It's not in the LUFA header.
	SPECIAL_HID_KEY_MOUSE_BTN5, 					   //  LOGICAL_KEY_KP_END,         ***
	SPECIAL_HID_KEY_MOUSE_BTN4,					  	   //  LOGICAL_KEY_KP_HOME,        ***
	SPECIAL_HID_KEY_MOUSE_BTN2,						   //  LOGICAL_KEY_KP_DELETE,      ***
	SPECIAL_HID_KEY_MOUSE_BTN1, 					   //  LOGICAL_KEY_KP_BACKSPACE,   ***
	HID_KEYBOARD_SC_LEFT_ALT, 					       //  LOGICAL_KEY_KP_L_ALT
	SPECIAL_HID_KEY_MOUSE_BTN3, 					   //  LOGICAL_KEY_KP_L_CTRL       ***
	HID_KEYBOARD_SC_BACKSLASH_AND_PIPE, 			   //  LOGICAL_KEY_KP_BACKSLASH,
	HID_KEYBOARD_SC_PAGE_DOWN, 						   //  LOGICAL_KEY_KP_PGDN,
	HID_KEYBOARD_SC_PAGE_UP, 						   //  LOGICAL_KEY_KP_PGUP,
	HID_KEYBOARD_SC_ENTER, 						   	   //  LOGICAL_KEY_KP_ENTER,
	HID_KEYBOARD_SC_KEYPAD_0_AND_INSERT,   		   	   //  LOGICAL_KEY_KP_SPACE, 	   ***
	HID_KEYBOARD_SC_RIGHT_ALT, 					   	   //  LOGICAL_KEY_KP_R_ALT,
	HID_KEYBOARD_SC_RIGHT_CONTROL, 				   	   //  LOGICAL_KEY_KP_R_CTRL,
	HID_KEYBOARD_SC_KEYPAD_PLUS, 			           //  LOGICAL_KEY_KP_SEMICOLON,   ***
	HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE, 			   //  LOGICAL_KEY_KP_QUOTE,
	HID_KEYBOARD_SC_KEYPAD_2_AND_DOWN_ARROW, 		   //  LOGICAL_KEY_KP_COMMA, 	   ***
	HID_KEYBOARD_SC_KEYPAD_3_AND_PAGE_DOWN, 		   //  LOGICAL_KEY_KP_PERIOD, 	   ***
	HID_KEYBOARD_SC_KEYPAD_ENTER, 		   			   //  LOGICAL_KEY_KP_SLASH, 	   ***
	HID_KEYBOARD_SC_RIGHT_SHIFT, 					   //  LOGICAL_KEY_KP_RSHIFT,
};


//////////////////////////////////////////////////////////////////////////////////////
// Kinesis board and uC Port Layout

// 74LS138 demultiplexer matrix select output:
//  A-C connect to the address pins 1-3 of the LS138s, externally pulled up.
//  P138SEL connects to pin 4 (negated input) of the first and pin 6 of the second,
//  to select between them, also externally pulled up. 
//  The opposing pins are tied to VCC and ground respectively.
//  Pin 10 ("WP") is connected to pin 5 of both demultiplexers, and must be pulled low to enable them.
//    A (pin 39) = C2
//    B (pin 38) = C4
//    C (pin 37) = C5
//    P138SEL (pin 36) = C6

// Key input:
//  Pressing the key pulls low. I don't think there's an external pull-up, so am using internal pull-ups.
//    keypad (pin 5) - D4,
//    program (pin 6) - C7,
//    matrix output (pins 21-28) - all of port B

// output:
//    4 LEDs (pins 1-4, sink current to enable) - D0-D3
//    buzzer (pin 32) = ???

// EEPROM access:
//  have access to up to two AT24C164 2kb serial eeproms via pins 7 and
//  8, connected to SCL and SDA respectively with external pull-ups.
//  The eeproms have their addresse lines tied to 000 and 001 respectively.
//         WP  (write protect) (pin10) = connected to WP pin of eeprom, must be low to enable. 
//                                       (note that this is also connected to the demultiplexers)
//         SCL (serial eeprom clock line) (pin7) = ???
//         SDA (serial eeprom data line) (pin8) = D7 (overlaps hardware button)

// unavailable-ish ports on uC:
//   d5 and d6 overlap onboard LEDB and A on my AT90USB162 dev board,
//   which means they're pulled up extremely strongly. I don't know
//   whether I can sink enough current to make a device I'm talking to
//   detect a 0.

// static configuration:
//   pin 20 - GND
//   pin 40 - VCC

// unused pins:
//   pin 11 - foot switch 1
//   pin 17 - foot switch 2 - note this is the older 2-foot-switch PS/2 model keyboard - do not know the layout of the newer one
//   pin 12 - PS/2 clock
//   pin 13 - PS/2 data
////////////////////////////////////////////////////////////////////////////////////////


#define MATRIX_PORT PORTC
#define MATRIX_DDR  DDRC
#define MATRIX_SELECT_A (1<<2)
#define MATRIX_SELECT_B (1<<4)
#define MATRIX_SELECT_C (1<<5)
#define MATRIX_SELECT_P138SEL (1<<6)
#define MATRIX_MASK (MATRIX_SELECT_A | MATRIX_SELECT_B | MATRIX_SELECT_C | MATRIX_SELECT_P138SEL)

#define INPUT_PIN5_PIN  PIND
#define INPUT_PIN5_PORT PORTD
#define INPUT_PIN5_DDR  DDRD

#define INPUT_PIN6_PIN  PINC
#define INPUT_PIN6_PORT PORTC
#define INPUT_PIN6_DDR  DDRC

#define INPUT_REST_PIN  PINB
#define INPUT_REST_PORT PORTB
#define INPUT_REST_DDR  DDRB

#define INPUT_PIN5 (1<<4)
#define INPUT_PIN6 (1<<7)
#define INPUT_REST (0xFF)

#define LED_PORT PORTD
#define LED_DDR  DDRD
#define LED1 (1)
#define LED2 (1<<1)
#define LED3 (1<<2)
#define LED4 (1<<3)
#define INT_LED1 (1<<5)
#define INT_LED2 (1<<6)
#define ALL_LEDS (LED1 | LED2 | LED3 | LED4 | INT_LED1 | INT_LED2)

/*
#define BUZZER_PORT PORTD
#define BUZZER_DDR DDRD
#define BUZZER (1<<4)

#define EEPROM_PORT PORTD
#define EEPROM_DDR  DDRD
#define EEPROM_SCL (1<<6)
#define EEPROM_SDA (1<<7)
*/

void ports_init(void){
	// Set up input: port C7 and all of port B 
	// we want to enable internal pull-ups on all of these pins - the multiplexer (or whatever else in the case of 5 and 6)
	// will pull its selected line low, and therefore if we test low, selected key is pressed, high, key is not pressed.
	INPUT_PIN5_DDR  &= ~INPUT_PIN5; // 0 = input pin
	INPUT_PIN5_PORT |=  INPUT_PIN5; // 1 = pull-up enabled
	INPUT_PIN6_DDR  &= ~INPUT_PIN6;
	INPUT_PIN6_PORT |=  INPUT_PIN6;
	INPUT_REST_DDR  &=  ~INPUT_REST;
	INPUT_REST_PORT |=   INPUT_REST;
	
	// Set up matrix selector output - C4, C5, C6.
	// this has an external pull-up, so we cause a low value by being a current sink, and a high by being high-impedence.
	// this corresponds to output low (PORT=0, DDR=1) or to input with no pullup (PORT=0, DDR=0)
	MATRIX_PORT &= ~MATRIX_MASK;
	MATRIX_DDR  |=  MATRIX_MASK; // start as output-low (sink current)

	// Set up LEDs - they're externally pulled up, so output-low(1,0) to enable, input-highz(0,0) to disable.
	LED_PORT &= ~(ALL_LEDS);
	LED_DDR  &= ~(ALL_LEDS); // start as hi-z (disabled)

	/*
	// don't know how the buzzer should be pulled to enable it, so start out high-impedance
	BUZZER_PORT &= ~BUZZER;
	BUZZER_DDR &= ~BUZZER;

	
	EEPROM_PORT &= ~(EEPROM_SCL | EEPROM_SDA | EEPROM_WP); // low/hi-z
	EEPROM_DDR  &= ~(EEPROM_SCL | EEPROM_SDA); 	// Initially, don't talk to the eeprom (input)
	*/
}

/**
 * Gets the current physical input for a given physical position
 */
void matrix_select_row(uint8_t matrix_row){
	// Select output using four bits starting at MATRIX_SELECT_A
	// set output with MATRIX_DDR - 0 means high (high-z input, external pullup), 1 means low (output low, current sink)

	uint8_t output_ddr_val = MATRIX_MASK; // start with all low level (current sink - 1)
	if(matrix_row & 0x1){
		output_ddr_val &= ~MATRIX_SELECT_A; // set DDR bit 0 for high level
	}
	if(matrix_row & 0x2){
		output_ddr_val &= ~MATRIX_SELECT_B;
	}
	if(matrix_row & 0x4){
		output_ddr_val &= ~MATRIX_SELECT_C;
	}
	if(matrix_row & 0x8){
		output_ddr_val &= ~MATRIX_SELECT_P138SEL;
	}
	
	MATRIX_DDR = (MATRIX_DDR & ~MATRIX_MASK) | output_ddr_val;
}

uint8_t matrix_read_column(uint8_t matrix_column){
	/*
	if(matrix_column >= MATRIX_COLS){
		panic();
	}
	*/

	if(matrix_column == 0){
		// read pin5
		uint8_t val = (INPUT_PIN5_PIN & INPUT_PIN5) == 0; // if high, button not pressed, if low, button pressed
		return val;
	}
	else if(matrix_column == 1){
		// read pin6
		uint8_t val = (INPUT_PIN6_PIN & INPUT_PIN6) == 0; // if high, button not pressed, if low, button pressed
		return val;
	}
	else{
		uint8_t shift = matrix_column - 2;
		uint8_t val = (INPUT_REST_PIN & (1<<shift)) == 0;
		return val;
	}
}

/* Macros: */
/** LED mask for the library LED driver, to indicate that the USB interface is not ready. */
#define LEDMASK_USB_NOTREADY     (LED4 | LED2)

/** LED mask for the library LED driver, to indicate that the USB interface is enumerating. */
#define LEDMASK_USB_ENUMERATING  (LED4 | LED1)

/** LED mask for the library LED driver, to indicate that the USB interface is ready. */
#define LEDMASK_USB_READY        0

/** LED mask for the library LED driver, to indicate that an error has occurred in the USB interface. */
#define LEDMASK_USB_ERROR        (LED4 | LED1 | LED2)

#define LEDMASK_CAPS      LED1
#define LEDMASK_NUMLOCK   LED2
#define LEDMASK_SCROLLLOCK LED3
#define LEDMASK_KEYPAD    LED4

#define LEDMASK_PROGRAMMING_SRC (LED1|LED3)
#define LEDMASK_PROGRAMMING_DST (LED2|LED3)
#define LEDMASK_ALL ALL_LEDS
#define LEDMASK_NONE 0

void set_all_leds(uint8_t led_mask){
	led_mask &= ALL_LEDS; // only touch within led range

	LED_DDR = (LED_DDR & ~ALL_LEDS) | led_mask;
}

void test_leds(void){
	for(int i = 0; i < 10; ++i){
		set_all_leds(0);
		_delay_ms(500);
		set_all_leds(LED1);
		_delay_ms(500);
		set_all_leds(LED2);
		_delay_ms(500);
		set_all_leds(LED3);
		_delay_ms(500);
		set_all_leds(LED4);
		_delay_ms(500);
		set_all_leds(INT_LED1);
		_delay_ms(500);
		set_all_leds(INT_LED2);

	}
}
