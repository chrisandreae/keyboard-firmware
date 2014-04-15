/* Name: usb.h
 * Project: Kinesis Ergonomic Keyboard Firmware Replacement
 * Author: Chris Andreae
 * Creation Date: 2012-10-17
 * Tabsize: 4
 * License: GNU GPL v2 (see GPL2.txt)
 */

#ifndef __USB_H
#define __USB_H
#include <stdint.h>

/**
 * Call from long-running operations to keep our watchdog and timers
 * up to date. Optionally call usbPoll() (with check to avoid
 * reentrancy).  Be aware that calling usbPoll() means that we could
 * hit our control transfer handlers, so avoid this if we're in a
 * state where this would be a bad thing.
 */
void USB_KeepAlive(uint8_t poll);

/**
 * Performs a USB update, scanning keyboard/mouse and responding
 * to interrupt requests. Includes KeepAlive.
 */
void USB_Perform_Update(void);

#endif // __USB_H
