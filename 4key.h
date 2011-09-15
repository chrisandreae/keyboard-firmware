
#include <LUFA/Drivers/Board/LEDs.h>
#include <LUFA/Drivers/Board/Buttons.h>

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
	HID_KEYBOARD_SC_LEFT_SHIFT,
	HID_KEYBOARD_SC_A,
	HID_KEYBOARD_SC_B
};

void ports_init(void){
	LEDs_Init();
	Buttons_Init();
}

/**
 * Gets the current physical input for a given physical position
 */
void matrix_select_row(uint8_t matrix_row){
	// do nothing
}

uint8_t matrix_read_column(uint8_t matrix_column){
	uint8_t button_status = Buttons_GetStatus();
	switch(matrix_column){
	case 0:
		return (button_status & BUTTONS_BUTTON1) != 0;
	case 1:
		return (button_status & BUTTONS_BUTTON2) != 0;
	case 2:
		return (button_status & BUTTONS_BUTTON3) != 0;
	case 3:
		return (button_status & BUTTONS_BUTTON4) != 0;
	default:
		return 0;
	}
}

/* Macros: */
/** LED mask for the library LED driver, to indicate that the USB interface is not ready. */
#define LEDMASK_USB_NOTREADY      LEDS_LED2

/** LED mask for the library LED driver, to indicate that the USB interface is enumerating. */
#define LEDMASK_USB_ENUMERATING  LEDS_LED1

/** LED mask for the library LED driver, to indicate that the USB interface is ready. */
#define LEDMASK_USB_READY        0

/** LED mask for the library LED driver, to indicate that an error has occurred in the USB interface. */
#define LEDMASK_USB_ERROR        (LEDS_LED1 | LEDS_LED2)

#define LEDMASK_CAPS      LEDS_LED1
#define LEDMASK_NUMLOCK   LEDS_LED2
#define LEDMASK_SCROLLOCK 0

#define LEDMASK_PROGRAMMING LEDS_ALL_LEDS
#define LEDMASK_ALL LEDS_ALL_LEDS
#define LEDMASK_NONE LEDS_NO_LEDS

void set_all_leds(uint8_t led_mask){
	LEDs_SetAllLEDs(led_mask);
}
