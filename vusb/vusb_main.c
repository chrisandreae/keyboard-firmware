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

#include <avr/pgmspace.h>   /* required by usbdrv.h */
#include "usbdrv.h"
#include "oddebug.h"        /* This is also an example for using debug macros */

#include "Keyboard.h"
#include "Descriptors.h"

// Use GCC built-in memory operations
#define memcmp(a,b,c) __builtin_memcmp(a,b,c)
#define memcpy(a,b,c) __builtin_memcpy(a,b,c)
#define bzero(x,y) __builtin_bzero(x,y)

/* ------------------------------------------------------------------------- */
/* ----------------------------- USB interface ----------------------------- */
/* ------------------------------------------------------------------------- */

static uint8_t idleRate		  = 0;   /* repeat rate for keyboard in 4ms increments - 0 means report only on changes */
static uint8_t reportProtocol = 1; // 1 = hid reports, 0 = boot protocol
static uint8_t expectReport = 0;

/** Global structure to hold the current keyboard interface HID report, for transmission to the host */
static KeyboardReport_Data_t KeyboardReportData;

/** Global structure to hold the current mouse interface HID report, for transmission to the host */
static MouseReport_Data_t MouseReportData;

/* ------------------------------------------------------------------------- */

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
				expectReport=1;
				return 0xFF; /* Call usbFunctionWrite with data */
			}
			break;
		case USBRQ_HID_GET_IDLE:
			usbMsgPtr = &idleRate;
			return 1;
		case USBRQ_HID_SET_IDLE:
			idleRate = rq->wValue.bytes[1];
			break;
		case USBRQ_HID_GET_PROTOCOL:
			usbMsgPtr = &reportProtocol;
			return 1;
		case USBRQ_HID_SET_PROTOCOL:
			reportProtocol = rq->wValue.bytes[1];
			break;
		}
	}else{
		/* no vendor specific requests implemented */
	}
	return 0;   /* default for not implemented requests: return no data back to host */
}

uchar usbFunctionWrite(uchar *data, uchar len) {
	if ((expectReport)&&(len==1)) {
		Process_KeyboardLEDReport(data[0]);
		expectReport=0;
		return 1;
	}
	expectReport=0;
	return 0x01;
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

	Update_Millis(elapsed);

	keyboard_idle_ms = (elapsed >= keyboard_idle_ms) ? 0 : keyboard_idle_ms - elapsed;
	mouse_idle_ms    = (elapsed >= mouse_idle_ms)    ? 0 : mouse_idle_ms - elapsed;
}

extern void Perform_USB_Update(int update_kbd, int update_mouse){
	wdt_reset();
	usbPoll();

	static bool sending_keyboard;
	static bool sending_mouse;

	bool kbd_idle_expired = idleRate && keyboard_idle_ms == 0;

	if(!sending_keyboard && (update_kbd || kbd_idle_expired)){
		bool must_send = kbd_idle_expired;

		if(update_kbd){
			memset(&KeyboardReportData, 0x00, sizeof(KeyboardReport_Data_t));
			must_send = must_send || Fill_KeyboardReport(&KeyboardReportData);
		}
		else if(kbd_idle_expired){
			KeyboardReportData = PrevKeyboardHIDReportBuffer;
		}

		if(idleRate){
			// reset idle time out
			keyboard_idle_ms = idleRate * 4;
		}

		if(must_send || 0 != memcmp(&PrevKeyboardHIDReportBuffer, &KeyboardReportData, sizeof(KeyboardReport_Data_t))){
			sending_keyboard = 1;
			PrevKeyboardHIDReportBuffer = KeyboardReportData;
		}
	}

	bool mouse_idle_expired = idleRate && mouse_idle_ms == 0;
	if(!sending_mouse && (update_mouse || mouse_idle_expired)){
		memset(&MouseReportData, 0x00, sizeof(MouseReport_Data_t));

		sending_mouse = mouse_idle_expired;

		if(update_mouse){
			sending_mouse = sending_mouse || Fill_MouseReport(&MouseReportData);
		}

		if(idleRate){
			mouse_idle_ms = idleRate * 4;
		}

	}

	if(sending_keyboard && usbInterruptIsReady()){
		usbSetInterrupt((void*)&KeyboardReportData, sizeof(KeyboardReportData));
		sending_keyboard = 0;
	}


	if(sending_mouse && usbInterruptIsReady3()){
		usbSetInterrupt3((void *)&MouseReportData, sizeof(MouseReportData));
		sending_mouse = 0;
	}

	update_uptimems();

}
