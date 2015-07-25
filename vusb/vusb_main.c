/* Name: vusb_main.c
 * Project: Kinesis Ergonomic Keyboard Firmware Replacement (v-usb)
 * Author: Chris Andreae
 * Creation Date: 2012-08-12
 * Tabsize: 4
 * License: GNU GPL v2 (see GPL2.txt)
 */

#include <avr/io.h>
#include <avr/wdt.h>
#include <avr/interrupt.h>  /* for sei() */
#include <util/delay.h>     /* for _delay_ms() */

#include "avr/eeprom.h"
#include <avr/pgmspace.h>   /* required by usbdrv.h */

#include "usbdrv.h"
#include "oddebug.h"        /* This is also an example for using debug macros */

#include "hardware.h"

#include "Keyboard.h"
#include "Descriptors.h"
#include "interpreter.h"
#include "config.h"
#include "buzzer.h"
#include "macro_index.h"
#include "macro.h"
#include "usb_vendor_interface.h"
#include "storage.h"

// Use GCC built-in memory operations
#define memcmp(a,b,c) __builtin_memcmp(a,b,c)
#define memcpy(a,b,c) __builtin_memcpy(a,b,c)
#define bzero(x,y) __builtin_bzero(x,y)

/* ------------------------------------------------------------------------- */
/* ----------------------------- USB interface ----------------------------- */
/* ------------------------------------------------------------------------- */

static uint8_t kbd_idleRate   = 0;   /* repeat rate for keyboard in 4ms increments - 0 means report only on changes */
static uint8_t mouse_idleRate = 0;   /* repeat rate for keyboard in 4ms increments - 0 means report only on changes */
static uint8_t reportProtocol = 1; // 1 = hid reports, 0 = boot protocol

/** Global structure to hold the current keyboard interface HID report, for transmission to the host */
static KeyboardReport_Data_t KeyboardReportData;

/** Global structure to hold the current mouse interface HID report, for transmission to the host */
static MouseReport_Data_t MouseReportData;

typedef enum _transfer_action {
	LED_REPORT,
	READ,
	WRITE,
} transfer_action;

static union {
	struct {
		transfer_action type;
		storage_type storage;
		uint8_t* addr;
		uint16_t remaining;
	} state;
	uint8_t byte;
	uint16_t word;
} transfer;

void(*transfer_callback)() = (void*) 0x0;


/* ------------------------------------------------------------------------- */

static uint16_t min_u16(uint16_t a, uint16_t b){
	return (a < b) ? a : b;
}

usbMsgLen_t usbFunctionSetup(uchar data[8]){
	usbRequest_t *rq = (void *)data;

	/* The following requests are never used. But since they are required by
	 * the specification, we implement them in this example.
	 */
	if((rq->bmRequestType & USBRQ_TYPE_MASK) == USBRQ_TYPE_CLASS){    /* class request type */
		switch(rq->bRequest){

		case USBRQ_HID_GET_REPORT:
			if(!rq->wIndex.word){ // wIndex specifies which interface we're talking about: 0 = kbd, 1 = mouse
				Fill_KeyboardReport(&KeyboardReportData); // We can assume that this isn't happening at the
														  // same time as interrupt in reports

				PrevKeyboardHIDReportBuffer = KeyboardReportData;

				usbMsgPtr = (void*)&KeyboardReportData;
				return sizeof(KeyboardReportData);
			}
			else{
				Fill_MouseReport(&MouseReportData);

				usbMsgPtr = (void*)&MouseReportData;
				return sizeof(MouseReportData);
			}
			break;
		case USBRQ_HID_SET_REPORT:
			if (!rq->wIndex.word && rq->wLength.word == 1) { /* We expect one byte reports for keyboard LEDs */
				transfer.state.type = LED_REPORT;
				return USB_NO_MSG; /* Call usbFunctionWrite with data */
			}
			break;
		case USBRQ_HID_GET_IDLE:
			if(rq->wIndex.word)
				usbMsgPtr = &mouse_idleRate;
			else
				usbMsgPtr = &kbd_idleRate;
			return 1;
		case USBRQ_HID_SET_IDLE:
			if(rq->wIndex.word)
				mouse_idleRate = rq->wValue.bytes[1];
			else
				kbd_idleRate = rq->wValue.bytes[1];
			break;
		case USBRQ_HID_GET_PROTOCOL:
			usbMsgPtr = &reportProtocol;
			return 1;
		case USBRQ_HID_SET_PROTOCOL:
			reportProtocol = rq->wValue.bytes[1];
			break;
		}
	}else{
		/* Vendor requests: */
		switch(rq->bRequest){

		/* byte sized transfers */

		case READ_NUM_PROGRAMS:
			transfer.byte = PROGRAM_COUNT;
			goto transfer_byte;
		case READ_LAYOUT_ID:
			transfer.byte = LAYOUT_ID;
			goto transfer_byte;
		case READ_MAPPING_SIZE:
			transfer.byte = NUM_LOGICAL_KEYS;
			goto transfer_byte;
		case READ_CONFIG_FLAGS: {
			configuration_flags fs = config_get_flags();
			transfer.byte = *((uint8_t*)&fs);
			goto transfer_byte;
		}
		case READ_MACRO_MAX_KEYS:
			transfer.byte = MACRO_MAX_KEYS;
			goto transfer_byte;
		transfer_byte:
			usbMsgPtr = &transfer.byte;
			return 1;

			/* Word sized transfers */

		case READ_PROGRAMS_SIZE:
			transfer.word = PROGRAM_SIZE;
			goto transfer_word;
		case READ_MACRO_INDEX_SIZE:
			transfer.word = MACRO_INDEX_SIZE;
			goto transfer_word;
		case READ_MACRO_STORAGE_SIZE:
			transfer.word = MACROS_SIZE;
			goto transfer_word;
		transfer_word:
			usbMsgPtr = (uint8_t*)&transfer.word;
			return 2;

			/* callback transfers */

		case WRITE_PROGRAMS:
			transfer.state.type = WRITE;
			transfer_callback = &vm_init;
			goto programs_rw;
		case READ_PROGRAMS:
			transfer.state.type = READ;
		programs_rw:
			transfer.state.storage = PROGRAM_STORAGE;
			transfer.state.addr = config_get_programs();
			transfer.state.remaining = rq->wLength.word;
			return USB_NO_MSG;


		case WRITE_MACRO_INDEX:
			transfer.state.type = WRITE;
			goto macro_index_rw;
		case READ_MACRO_INDEX:
			transfer.state.type = READ;
		macro_index_rw:
			transfer.state.storage = MACRO_INDEX_STORAGE;
			transfer.state.addr = macro_idx_get_storage();
			transfer.state.remaining = rq->wLength.word;
			return USB_NO_MSG;

		case WRITE_MACRO_STORAGE:
			transfer.state.type = WRITE;
			goto macro_storage_rw;
		case READ_MACRO_STORAGE:
			transfer.state.type = READ;
		macro_storage_rw:
			transfer.state.storage = MACROS_STORAGE;
			transfer.state.addr = macros_get_storage();
			transfer.state.remaining = rq->wLength.word;
			return USB_NO_MSG;

		case WRITE_CONFIG_FLAGS: {
			uint8_t b = (rq->wValue.word & 0xff);
			config_save_flags(*(configuration_flags*)&b);
			break;
		}
		case WRITE_MAPPING:
			transfer.state.type = WRITE;
			goto mapping_rw1;
		case READ_DEFAULT_MAPPING:
			transfer.state.type = READ;
			transfer.state.storage = CONSTANT_STORAGE;
			transfer.state.addr = (uint8_t*) logical_to_hid_map_default;
			goto mapping_rw2;
		case READ_MAPPING:
			transfer.state.type = READ;
		mapping_rw1:
			transfer.state.storage = MAPPING_STORAGE;
			transfer.state.addr = config_get_mapping();
		mapping_rw2:
			transfer.state.remaining = min_u16(NUM_LOGICAL_KEYS, rq->wLength.word);
			return USB_NO_MSG;

		case RESET_DEFAULTS:
			config_reset_defaults();
			break;

		case RESET_FULLY:
			config_reset_fully();
			break;
		}
	}
	return 0;   /* default for not implemented requests: return no data back to host */
}

uint8_t* step_addr;

// Receive information from computer. Return 0 or 1 to tell the driver
// whether we are finished with this transfer.
uchar usbFunctionWrite(uchar *data, uchar len) {
	uint8_t write_sz = len <= transfer.state.remaining ? len : transfer.state.remaining;
	uint8_t ret = 1;

	switch(transfer.state.type){
	case WRITE: {
		storage_err (*write_fn)(void*, const void*, uint8_t, uint8_t);

		switch(transfer.state.storage){
		case avr_eeprom:
			write_fn = &avr_eeprom_write_step;
			break;
		case i2c_eeprom:
			write_fn = &i2c_eeprom_write_step;
			break;
		default:
			goto err;
		}

		storage_err r = write_fn(transfer.state.addr, data, write_sz, transfer.state.remaining == write_sz);
		if(r != SUCCESS){ goto err; }

		transfer.state.addr += write_sz;
		transfer.state.remaining -= write_sz;
		ret = (transfer.state.remaining == 0);
		break;
	}
	case LED_REPORT:
		Process_KeyboardLEDReport(data[0]);
		break;
	default:
		break;
	}

	if(ret == 1 && transfer_callback){
		transfer_callback();
		transfer_callback = (void*) 0x0;
	}
	return ret;

 err:
	buzzer_start(200);
	return 1; // end transfer if failed
}

#include <buzzer.h>

// Send information back to the computer
// returns bytes put in buffer
uchar usbFunctionRead(uchar* data, uchar len) {
	uint8_t read_sz = len <= transfer.state.remaining ? len : transfer.state.remaining;
	size_t r;

	switch(transfer.state.type){
	case READ: {
		size_t (*read_fn)(const void*, void*, size_t);

		switch(transfer.state.storage){
		case avr_eeprom:
			read_fn = &avr_eeprom_read;
			break;
		case avr_pgm:
			read_fn = &avr_pgm_read;
			break;
		case i2c_eeprom:
			read_fn = &i2c_eeprom_read;
			break;
		default:
			goto err;
		}

		r = read_fn(transfer.state.addr, data, read_sz);
		if(r == -1) goto err;

		transfer.state.addr += r;
		transfer.state.remaining -= r;
		return r;
	}
	default:
		return 0;
	}

 err:
	buzzer_start(200);
	return 0; // no bytes sent, error
}


#if 0 // receive LED state via interrupt OUT endpoint 2
/* This function is called by the driver when data is received on an interrupt-
 * or bulk-out endpoint. The endpoint number can be found in the global
 * variable usbRxToken. You must define USB_CFG_IMPLEMENT_FN_WRITEOUT to 1 in
 * usbconfig.h to get this function called.
 */
void usbFunctionWriteOut(uchar *data, uchar len){
	if(usbRxToken == 2 && len == 1){
		Process_KeyboardLEDReport(data[0]);
	}
}
#endif

int main(void) {
	uchar   i;

	wdt_enable(WDTO_1S);
	/* Even if you don't use the watchdog, turn it off here. On newer devices,
	 * the status of the watchdog (on/off, period) is PRESERVED OVER RESET!
	 */
	/* RESET status: all port bits are inputs without pull-up.
	 * That's the way we need D+ and D-. Therefore we don't need any
	 * additional hardware initialization.
	 */
	odDebugInit();
	DBG1(0x00, 0, 0);       /* debug output: main starts */

	TCCR1B |= ((1 << CS10) | (1 << CS11)); // Set up timer at Fcpu/64

	usbInit();
	usbDeviceDisconnect();  /* enforce re-enumeration, do this while interrupts are disabled! */
	i = 0;
	while(--i){             /* fake USB disconnect for > 250 ms */
		wdt_reset();
		_delay_ms(1);
	}
	usbDeviceConnect();

	Keyboard_Main();
}

static uint8_t keyboard_idle_ms = 0;
static uint8_t mouse_idle_ms = 0;

void update_uptimems(){
	// We're called every time around the loop, so here's where we update uptimems for our next time.  If we
	// run a prescaled timer at CPU/64 (250khz) we can record up to a third of second of change, so we just
	// work out how many millis have passed (t/250) and add to uptimems (saving the remainder between runs)
	static uint8_t remainder = 0;

	uint16_t elapsed_ticks = TCNT1;
	TCNT1 = 0;

	elapsed_ticks += remainder;

	uint16_t elapsed = elapsed_ticks / 250;
	remainder = elapsed_ticks % 250;

	if(elapsed > 0){
		Update_Millis(elapsed);
	}

	keyboard_idle_ms = (elapsed >= keyboard_idle_ms) ? 0 : keyboard_idle_ms - elapsed;
	mouse_idle_ms    = (elapsed >= mouse_idle_ms)    ? 0 : mouse_idle_ms - elapsed;
}

bool update_and_compare(void* buf, void* prev_buf, size_t buflen, void(*fill_fn)(void*)){
	memset(buf, 0x0, buflen);
	fill_fn(buf);
	if(0 == memcmp(buf, prev_buf, buflen)){
		// Buffers are the same
		return 0;
	}
	else{
		// buffers are different
		memcpy(prev_buf, buf, buflen);
		return 1;
	}
}

/**
 * Call from long-running operations to keep our watchdog and timers
 * up to date. Optionally call usbPoll() (with check to avoid
 * reentrancy).  Be aware that calling usbPoll() means that we could
 * hit our control transfer handlers, so avoid this if we're in a
 * state where this would be a bad thing.
 */
void USB_KeepAlive(uint8_t poll){
	// Tend to our timers
	wdt_reset();
	update_uptimems();

	// and usbpoll if we're not already doing so
	static uint8_t in_poll = 0;
	if(poll && !in_poll){
		in_poll = 1;
		usbPoll();
		in_poll = 0;
	}
}

void USB_Perform_Update(void){
	USB_KeepAlive(true);

	static bool sending_keyboard = 0;
	static bool sending_mouse = 0;

	// Keyboard
	if(!sending_keyboard){
		// Update, set sending_keyboard if the report is different to last time
		sending_keyboard = update_and_compare(&KeyboardReportData, &PrevKeyboardHIDReportBuffer, sizeof(KeyboardReport_Data_t), (void(*)(void*)) &Fill_KeyboardReport);
	}
	if(!sending_keyboard && (kbd_idleRate && keyboard_idle_ms == 0)){
		// if still not sending and expired, re-send the previous buffer
		KeyboardReportData = PrevKeyboardHIDReportBuffer;
		sending_keyboard = 1;
	}

	if(sending_keyboard && usbInterruptIsReady()){
		usbSetInterrupt((void*)&KeyboardReportData, sizeof(KeyboardReportData));
		sending_keyboard = 0;
		// now that we've sent, reset the idle timer
		keyboard_idle_ms = kbd_idleRate * 4;
	}

	// Mouse
	if(!sending_mouse){
		// Update, set sending_mouse if the report is different to last time
		sending_mouse = update_and_compare(&MouseReportData, &PrevMouseHIDReportBuffer, sizeof(MouseReport_Data_t), (void(*)(void*)) &Fill_MouseReport);
		// Mouse reports need to repeat for the cursor to continue to move, so always send if there is a movement component to the report
		if(MouseReportData.X || MouseReportData.Y || MouseReportData.Wheel){
			sending_mouse = 1;
		}
	}
	if(!sending_mouse && (mouse_idleRate && mouse_idle_ms == 0)){
		// if still not sending and expired, re-send the previous buffer
		MouseReportData = PrevMouseHIDReportBuffer;
		sending_mouse = 1;
	}

	if(sending_mouse && usbInterruptIsReady3()){
		usbSetInterrupt3((void *)&MouseReportData, sizeof(MouseReportData));
		sending_mouse = 0;
		mouse_idle_ms = mouse_idleRate * 4;
	}

	// and update the timer again
	update_uptimems();
}
