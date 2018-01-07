/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  Licensed under the GNU GPL v2 (see GPL2.txt).

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

#include <util/delay.h>     /* for _delay_ms() */
#include "keypad.h"


#define NO_LHS_KEY NO_KEY
#define NO_RHS_KEY NO_KEY

// Note that the matrix defined here is sideways with respect to
// matrix_select_row/matrix_read_column.  This is because keystate.c assumes we
// can drive rows and read columns, but the shift registers in this keyboard
// are installed so that we drive columns and read rows.
const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM = {
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C7R6, LOGICAL_KEY_RHS_C7R6, LOGICAL_KEY_LHS_C7R5, LOGICAL_KEY_RHS_C7R5, LOGICAL_KEY_LHS_C7R4, LOGICAL_KEY_RHS_C7R4, LOGICAL_KEY_LHS_C7R3, LOGICAL_KEY_RHS_C7R3, LOGICAL_KEY_LHS_C7R2, LOGICAL_KEY_RHS_C7R2, LOGICAL_KEY_LHS_C7R1, LOGICAL_KEY_RHS_C7R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C6R6, LOGICAL_KEY_RHS_C6R6, LOGICAL_KEY_LHS_C6R5, LOGICAL_KEY_RHS_C6R5, LOGICAL_KEY_LHS_C6R4, LOGICAL_KEY_RHS_C6R4, LOGICAL_KEY_LHS_C6R3, LOGICAL_KEY_RHS_C6R3, LOGICAL_KEY_LHS_C6R2, LOGICAL_KEY_RHS_C6R2, LOGICAL_KEY_LHS_C6R1, LOGICAL_KEY_RHS_C6R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C5R6, LOGICAL_KEY_RHS_C5R6, LOGICAL_KEY_LHS_C5R5, LOGICAL_KEY_RHS_C5R5, LOGICAL_KEY_LHS_C5R4, LOGICAL_KEY_RHS_C5R4, LOGICAL_KEY_LHS_C5R3, LOGICAL_KEY_RHS_C5R3, LOGICAL_KEY_LHS_C5R2, LOGICAL_KEY_RHS_C5R2, LOGICAL_KEY_LHS_C5R1, LOGICAL_KEY_RHS_C5R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C4R6, LOGICAL_KEY_RHS_C4R6, LOGICAL_KEY_LHS_C4R5, LOGICAL_KEY_RHS_C4R5, LOGICAL_KEY_LHS_C4R4, LOGICAL_KEY_RHS_C4R4, LOGICAL_KEY_LHS_C4R3, LOGICAL_KEY_RHS_C4R3, LOGICAL_KEY_LHS_C4R2, LOGICAL_KEY_RHS_C4R2, LOGICAL_KEY_LHS_C4R1, LOGICAL_KEY_RHS_C4R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C3R6, LOGICAL_KEY_RHS_C3R6, LOGICAL_KEY_LHS_C3R5, LOGICAL_KEY_RHS_C3R5, LOGICAL_KEY_LHS_C3R4, LOGICAL_KEY_RHS_C3R4, LOGICAL_KEY_LHS_C3R3, LOGICAL_KEY_RHS_C3R3, LOGICAL_KEY_LHS_C3R2, LOGICAL_KEY_RHS_C3R2, LOGICAL_KEY_LHS_C3R1, LOGICAL_KEY_RHS_C3R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C2R6, LOGICAL_KEY_RHS_C2R6, LOGICAL_KEY_LHS_C2R5, LOGICAL_KEY_RHS_C2R5, LOGICAL_KEY_LHS_C2R4, LOGICAL_KEY_RHS_C2R4, LOGICAL_KEY_LHS_C2R3, LOGICAL_KEY_RHS_C2R3, LOGICAL_KEY_LHS_C2R2, LOGICAL_KEY_RHS_C2R2, LOGICAL_KEY_LHS_C2R1, LOGICAL_KEY_RHS_C2R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_C1R6, LOGICAL_KEY_RHS_C1R6, LOGICAL_KEY_LHS_C1R5, LOGICAL_KEY_RHS_C1R5, LOGICAL_KEY_LHS_C1R4, LOGICAL_KEY_RHS_C1R4, LOGICAL_KEY_LHS_C1R3, LOGICAL_KEY_RHS_C1R3, LOGICAL_KEY_LHS_C1R2, LOGICAL_KEY_RHS_C1R2, LOGICAL_KEY_LHS_C1R1, LOGICAL_KEY_RHS_C1R1},
    {NO_LHS_KEY, NO_RHS_KEY, NO_LHS_KEY, NO_RHS_KEY, LOGICAL_KEY_LHS_TP3 , LOGICAL_KEY_RHS_TP3 , LOGICAL_KEY_LHS_TP2 , LOGICAL_KEY_RHS_TP2 , LOGICAL_KEY_LHS_TP1 , LOGICAL_KEY_RHS_TP1 , LOGICAL_KEY_LHS_TP6 , LOGICAL_KEY_RHS_TP6 , LOGICAL_KEY_LHS_TP5 , LOGICAL_KEY_RHS_TP5 , LOGICAL_KEY_LHS_TP4 , LOGICAL_KEY_RHS_TP4 },
};

const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM = {
    // Right hand side

    // Column (thumbpad)
    HID_KEYBOARD_SC_RIGHT_CONTROL,
    HID_KEYBOARD_SC_RIGHT_GUI,
    HID_KEYBOARD_SC_RIGHT_ALT,
    HID_KEYBOARD_SC_SPACE,
    HID_KEYBOARD_SC_ENTER,
    HID_KEYBOARD_SC_RIGHT_SHIFT,

    // Column 1
    NO_KEY,
    HID_KEYBOARD_SC_6_AND_CARET,
    HID_KEYBOARD_SC_Y,
    HID_KEYBOARD_SC_H,
    HID_KEYBOARD_SC_N,
    NO_KEY,

    // Col 2
    HID_KEYBOARD_SC_F5,
    HID_KEYBOARD_SC_7_AND_AND_AMPERSAND,
    HID_KEYBOARD_SC_U,
    HID_KEYBOARD_SC_J,
    HID_KEYBOARD_SC_M,
    HID_KEYBOARD_SC_UP_ARROW,

    // Col 3
    HID_KEYBOARD_SC_F6,
    HID_KEYBOARD_SC_8_AND_ASTERISK,
    HID_KEYBOARD_SC_I,
    HID_KEYBOARD_SC_K,
    HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN,
    HID_KEYBOARD_SC_DOWN_ARROW,

    // Col 4
    HID_KEYBOARD_SC_F7,
    HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS,
    HID_KEYBOARD_SC_O,
    HID_KEYBOARD_SC_L,
    HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN,
    HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE,

    // Col 5
    HID_KEYBOARD_SC_F8,
    HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS,
    HID_KEYBOARD_SC_P,
    HID_KEYBOARD_SC_SEMICOLON_AND_COLON,
    HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK,
    HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE,

    // Col 6
    SPECIAL_HID_KEY_PROGRAM,
    HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE,
    HID_KEYBOARD_SC_BACKSLASH_AND_PIPE,
    HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE,
    HID_KEYBOARD_SC_PAGE_UP,
    HID_KEYBOARD_SC_PAGE_DOWN,

    // Col 7 (header)
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,

    // Left hand side

    // Column (thumbpad)
    HID_KEYBOARD_SC_LEFT_ALT,
    HID_KEYBOARD_SC_LEFT_GUI,
    HID_KEYBOARD_SC_LEFT_CONTROL,
    HID_KEYBOARD_SC_BACKSPACE,
    HID_KEYBOARD_SC_DELETE,
    HID_KEYBOARD_SC_LEFT_SHIFT,

    // Column 1
    NO_KEY,
    HID_KEYBOARD_SC_5_AND_PERCENTAGE,
    HID_KEYBOARD_SC_T,
    HID_KEYBOARD_SC_G,
    HID_KEYBOARD_SC_B,
    NO_KEY,

    // Col 2
    HID_KEYBOARD_SC_F4,
    HID_KEYBOARD_SC_4_AND_DOLLAR,
    HID_KEYBOARD_SC_R,
    HID_KEYBOARD_SC_F,
    HID_KEYBOARD_SC_V,
    HID_KEYBOARD_SC_RIGHT_ARROW,

    // Col 3
    HID_KEYBOARD_SC_F3,
    HID_KEYBOARD_SC_3_AND_HASHMARK,
    HID_KEYBOARD_SC_E,
    HID_KEYBOARD_SC_D,
    HID_KEYBOARD_SC_C,
    HID_KEYBOARD_SC_LEFT_ARROW,

    // Col 4
    HID_KEYBOARD_SC_F2,
    HID_KEYBOARD_SC_2_AND_AT,
    HID_KEYBOARD_SC_W,
    HID_KEYBOARD_SC_S,
    HID_KEYBOARD_SC_X,
    HID_KEYBOARD_SC_CAPS_LOCK,

    // Col 5
    HID_KEYBOARD_SC_F1,
    HID_KEYBOARD_SC_1_AND_EXCLAMATION,
    HID_KEYBOARD_SC_Q,
    HID_KEYBOARD_SC_A,
    HID_KEYBOARD_SC_Z,
    HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE,

    // Col 6
    HID_KEYBOARD_SC_ESCAPE,
    HID_KEYBOARD_SC_EQUAL_AND_PLUS,
    HID_KEYBOARD_SC_TAB,
    SPECIAL_HID_KEY_KEYPAD_SHIFT,
    HID_KEYBOARD_SC_HOME,
    HID_KEYBOARD_SC_END,

    // Col 7 (header)
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,

    // Keypad Layer, Right hand side

    // Column (thumbpad)
    HID_KEYBOARD_SC_RIGHT_CONTROL,
    HID_KEYBOARD_SC_RIGHT_GUI,
    HID_KEYBOARD_SC_RIGHT_ALT,
    HID_KEYBOARD_SC_KEYPAD_0_AND_INSERT,
    HID_KEYBOARD_SC_KEYPAD_DOT_AND_DELETE,
    HID_KEYBOARD_SC_RIGHT_SHIFT,

    // Column 1
    NO_KEY,
    HID_KEYBOARD_SC_INSERT,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,

    // Col 2
    HID_KEYBOARD_SC_PRINT_SCREEN,
    HID_KEYBOARD_SC_NUM_LOCK,
    HID_KEYBOARD_SC_KEYPAD_7_AND_HOME,
    HID_KEYBOARD_SC_KEYPAD_4_AND_LEFT_ARROW,
    HID_KEYBOARD_SC_KEYPAD_1_AND_END,
    HID_KEYBOARD_SC_UP_ARROW,

    // Col 3
    HID_KEYBOARD_SC_PAUSE,
    HID_KEYBOARD_SC_KEYPAD_EQUAL_SIGN,
    HID_KEYBOARD_SC_KEYPAD_8_AND_UP_ARROW,
    HID_KEYBOARD_SC_KEYPAD_5,
    HID_KEYBOARD_SC_KEYPAD_2_AND_DOWN_ARROW,
    HID_KEYBOARD_SC_DOWN_ARROW,

    // Col 4
    HID_KEYBOARD_SC_SCROLL_LOCK,
    HID_KEYBOARD_SC_KEYPAD_SLASH,
    HID_KEYBOARD_SC_KEYPAD_9_AND_PAGE_UP,
    HID_KEYBOARD_SC_KEYPAD_6_AND_RIGHT_ARROW,
    HID_KEYBOARD_SC_KEYPAD_3_AND_PAGE_DOWN,
    NO_KEY,

    // Col 5
    NO_KEY,
    HID_KEYBOARD_SC_KEYPAD_ASTERISK,
    HID_KEYBOARD_SC_KEYPAD_MINUS,
    HID_KEYBOARD_SC_KEYPAD_PLUS,
    HID_KEYBOARD_SC_KEYPAD_ENTER,
    HID_KEYBOARD_SC_KEYPAD_ENTER,

    // Col 6
    SPECIAL_HID_KEY_PROGRAM,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,

    // Col 7 (header)
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,

    // Keypad Layer, left hand side (not yet defined!)

    // Column (thumbpad)
    HID_KEYBOARD_SC_LEFT_ALT,
    HID_KEYBOARD_SC_LEFT_GUI,
    HID_KEYBOARD_SC_LEFT_CONTROL,
    SPECIAL_HID_KEY_MOUSE_BTN1,
    SPECIAL_HID_KEY_MOUSE_BTN2,
    HID_KEYBOARD_SC_LEFT_SHIFT,

    // Column 1
    NO_KEY,
    HID_KEYBOARD_SC_VOLUME_UP,
    HID_KEYBOARD_SC_VOLUME_DOWN,
    HID_KEYBOARD_SC_COPY,
    HID_KEYBOARD_SC_PASTE,
    NO_KEY,

    // Col 2
    HID_KEYBOARD_SC_F12,
    HID_KEYBOARD_SC_MUTE,
    NO_KEY,
    SPECIAL_HID_KEY_MOUSE_RIGHT,
    HID_KEYBOARD_SC_CUT,
    HID_KEYBOARD_SC_RIGHT_ARROW,

    // Col 3
    HID_KEYBOARD_SC_F11,
    NO_KEY,
    SPECIAL_HID_KEY_MOUSE_FWD,
    SPECIAL_HID_KEY_MOUSE_BACK,
    NO_KEY,
    HID_KEYBOARD_SC_LEFT_ARROW,

    // Col 4
    HID_KEYBOARD_SC_F10,
    NO_KEY,
    NO_KEY,
    SPECIAL_HID_KEY_MOUSE_LEFT,
    NO_KEY,
    NO_KEY,

    // Col 5
    HID_KEYBOARD_SC_F9,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    0x65, // Windows 'menu' key

    // Col 6
    NO_KEY,
    NO_KEY,
    NO_KEY,
    SPECIAL_HID_KEY_KEYPAD_SHIFT,
    NO_KEY,
    NO_KEY,

    // Col 7 (header)
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
    NO_KEY,
};

void ports_init(void){
    // Set up output
    P2S_LOAD_S2P_SER_DDR  |= P2S_LOAD_S2P_SER_MASK; // 1 = output
    P2S_LOAD_S2P_SER_PORT |= P2S_LOAD_S2P_SER_MASK; // 1

    P2S_CLOCK_DDR  |= P2S_CLOCK_MASK; // 1 = output
    P2S_CLOCK_PORT |= P2S_CLOCK_MASK; // 1

    S2P_CLOCK_DDR  |= S2P_CLOCK_MASK; // 1 = output
    S2P_CLOCK_PORT |= S2P_CLOCK_MASK; // 1

    // Set up input
    RHS_P2S_DATA_DDR  &= ~RHS_P2S_DATA_MASK; // 0 = input
    RHS_P2S_DATA_PORT &= ~RHS_P2S_DATA_MASK; // high-z

    LHS_P2S_DATA_DDR  &= ~LHS_P2S_DATA_MASK; // 0 = input
    LHS_P2S_DATA_PORT &= ~LHS_P2S_DATA_MASK; // high-z

    // LEDs sink current: output-low (1,0) to enable, input-highz (0,0) to disable
    LED_DDR  &= ~ALL_LEDS; // start as 0,0
    LED_PORT &= ~ALL_LEDS;
}

void matrix_select_row(uint8_t matrix_row){
    // clock in the target row to the S2P register
    for(uint8_t i = 0; i < 8; ++i){
        if((7 - i) == matrix_row){
            // target bit
            P2S_LOAD_S2P_SER_PORT |= P2S_LOAD_S2P_SER_MASK;
        }
        else{
            P2S_LOAD_S2P_SER_PORT &= ~P2S_LOAD_S2P_SER_MASK;
        }
        // clock in the bit
        S2P_CLOCK_PORT &= ~S2P_CLOCK_MASK;  // 0
        S2P_CLOCK_PORT |=  S2P_CLOCK_MASK;  // 1
    }

    // load the columns of the selected row into the P2S register
    P2S_LOAD_S2P_SER_PORT |= P2S_LOAD_S2P_SER_MASK;
    P2S_LOAD_S2P_SER_PORT &= ~P2S_LOAD_S2P_SER_MASK;
    P2S_LOAD_S2P_SER_PORT |= P2S_LOAD_S2P_SER_MASK;
}

uint8_t matrix_read_column(uint8_t matrix_column){
    uint8_t val;
    if((matrix_column % 2) == 0){
        // Read the current bit in Q7
        val = (LHS_P2S_DATA_PIN & LHS_P2S_DATA_MASK) != 0;
    }
    else{
        val = (RHS_P2S_DATA_PIN & RHS_P2S_DATA_MASK) != 0;

        // clock in the next bit for both sides
        P2S_CLOCK_PORT &= ~P2S_CLOCK_MASK;
        P2S_CLOCK_PORT |= P2S_CLOCK_MASK;
    }
    return val;
}

void set_all_leds(uint8_t led_mask){
    led_mask &= ALL_LEDS; // only touch within led range

    LED_DDR = (LED_DDR & ~ALL_LEDS) | led_mask;
}
