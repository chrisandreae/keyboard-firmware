// keyboard settings

#define NUM_LOGICAL_KEYS 32
#define MATRIX_COLS 4
#define MATRIX_ROWS 1
#define MATRIX_SIZE (MATRIX_COLS * MATRIX_ROWS)

// Logical keys we have
enum logical_keys {
	LOGICAL_KEY_PROGRAM,
	LOGICAL_KEY_LSHIFT,
	LOGICAL_KEY_A,
	LOGICAL_KEY_B,
	// and extra logical keys that need to be defined for keyboard.c to not complain:
	LOGICAL_KEY_F11,
	LOGICAL_KEY_F12,
	LOGICAL_KEY_HYPHEN,
	LOGICAL_KEY_F7,
	LOGICAL_KEY_S,
	LOGICAL_KEY_L,
	LOGICAL_KEY_D,
	LOGICAL_KEY_1,
	LOGICAL_KEY_0
};

// because the matrix is not tightly packed, we want a map from matrix
// position to logical key.
const logical_keycode matrix_to_logical_map[MATRIX_ROWS][MATRIX_COLS] PROGMEM = {
	{ LOGICAL_KEY_PROGRAM, LOGICAL_KEY_LSHIFT, LOGICAL_KEY_A, LOGICAL_KEY_B },
};

/* For each key, maps an index position to a default HID key code. */
/* stored in flash. */
const hid_keycode logical_to_hid_map_default[NUM_LOGICAL_KEYS] PROGMEM = {
	SPECIAL_HID_KEY_PROGRAM,
	SPECIAL_HID_KEY_MOUSE_BACK,
	HID_KEYBOARD_SC_A,
	HID_KEYBOARD_SC_CAPS_LOCK
};

#define LED_PORT PORTC
#define LED_DDR  DDRC
#define LED1 (1<<4)
#define LED2 (1<<5)
#define ALL_LEDS (LED1 | LED2)

#define INPUT_PORT PORTC
#define INPUT_PIN PINC
#define INPUT_DDR DDRC
#define INPUT_MASK 0x0f


void ports_init(void){
	// we want to enable internal pull-ups on all of these pins - pull low to press
	INPUT_DDR &= ~INPUT_MASK; // 0: input pin
	INPUT_PORT |= INPUT_MASK; // 1: pull-up enabled

	// Set up LEDs - they're externally pulled up, so output-low(1,0) to enable, input-highz(0,0) to disable.
	LED_PORT &= ~(ALL_LEDS);
	LED_DDR  &= ~(ALL_LEDS); // start as hi-z (disabled)
}

/**
 * Gets the current physical input for a given physical position
 */
void matrix_select_row(uint8_t matrix_row){
	// do nothing
}

uint8_t matrix_read_column(uint8_t matrix_column){
	return (INPUT_PIN & (1<<matrix_column)) == 0; // if high, button not pressed; if low, button pressed.
}

/* Macros: */


/** LED mask for the library LED driver, to indicate that the USB interface is not ready. */
#define LEDMASK_USB_NOTREADY      LED2

/** LED mask for the library LED driver, to indicate that the USB interface is enumerating. */
#define LEDMASK_USB_ENUMERATING  LED1

/** LED mask for the library LED driver, to indicate that the USB interface is ready. */
#define LEDMASK_USB_READY        0

/** LED mask for the library LED driver, to indicate that an error has occurred in the USB interface. */
#define LEDMASK_USB_ERROR        (LED1 | LED2)

#define LEDMASK_CAPS      LED1
#define LEDMASK_NUMLOCK   LED2
#define LEDMASK_SCROLLLOCK 0

#define LEDMASK_PROGRAMMING_SRC ALL_LEDS
#define LEDMASK_PROGRAMMING_DST ALL_LEDS
#define LEDMASK_ALL ALL_LEDS
#define LEDMASK_NONE NO_LEDS

void set_all_leds(uint8_t led_mask){
	led_mask &= ALL_LEDS; // only touch within led range

	LED_DDR = (LED_DDR & ~ALL_LEDS) | led_mask;
}
