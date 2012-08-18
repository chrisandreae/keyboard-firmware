/*
  Kinesis ergonomic keyboard firmware replacement (LUFA)

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  This program includes library and sample code from:

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

#include "Keyboard.h"

#include <LUFA/Version.h>
#include <LUFA/Drivers/USB/USB.h>

/** LUFA HID Class driver interface configuration and state information. This structure is
 *  passed to all HID Class driver functions, so that multiple instances of the same class
 *  within a device can be differentiated from one another.
 */
USB_ClassInfo_HID_Device_t Keyboard_HID_Interface =
	{
		.Config =
			{
				.InterfaceNumber              = 0,

				.ReportINEndpointNumber       = KEYBOARD_IN_EPNUM,
				.ReportINEndpointSize         = HID_EPSIZE,
				.ReportINEndpointDoubleBank   = false,

				.PrevReportINBuffer           = (void*) &PrevKeyboardHIDReportBuffer,
				.PrevReportINBufferSize       = sizeof(PrevKeyboardHIDReportBuffer),
			},
	};

/** LUFA HID Class driver interface configuration and state information. This structure is
 *  passed to all HID Class driver functions, so that multiple instances of the same class
 *  within a device can be differentiated from one another. This is for the mouse HID
 *  interface within the device.
 */
USB_ClassInfo_HID_Device_t Mouse_HID_Interface =
	{
		.Config =
			{
				.InterfaceNumber              = 1,

				.ReportINEndpointNumber       = MOUSE_IN_EPNUM,
				.ReportINEndpointSize         = HID_EPSIZE,

				.PrevReportINBuffer           = (void*) &PrevMouseHIDReportBuffer,
				.PrevReportINBufferSize       = sizeof(PrevMouseHIDReportBuffer),
			},
	};

int main(void) {
	/* Disable watchdog if enabled by bootloader/fuses */
	MCUSR &= ~(1 << WDRF);
	wdt_disable();

	/* Disable clock division */
	clock_prescale_set(clock_div_1);

	/* Hardware Initialization */
	Update_USBState(NOTREADY);

	USB_Init();


	// and enter the keyboard loop
	Keyboard_Main();

	return 0;
}

void Perform_USB_Update(int update_kbd, int update_mouse){
	if(update_mouse)
		HID_Device_USBTask(&Mouse_HID_Interface);

	if(update_kbd)
		HID_Device_USBTask(&Keyboard_HID_Interface);

	USB_USBTask();
}

/** Event handler for the library USB Connection event. */
void EVENT_USB_Device_Connect(void)
{
	Update_USBState(ENUMERATING);
}

/** Event handler for the library USB Disconnection event. */
void EVENT_USB_Device_Disconnect(void)
{
	Update_USBState(NOTREADY);
}

/** Event handler for the library USB Configuration Changed event. */
void EVENT_USB_Device_ConfigurationChanged(void)
{
	bool ConfigSuccess = true;

	ConfigSuccess &= HID_Device_ConfigureEndpoints(&Keyboard_HID_Interface);

	ConfigSuccess &= HID_Device_ConfigureEndpoints(&Mouse_HID_Interface);

	// enable the start-of-frame event (millisecond callback)
	USB_Device_EnableSOFEvents();

	Update_USBState(ConfigSuccess ? READY : ERROR);
}

/** Event handler for the library USB Control Request reception event. */
void EVENT_USB_Device_ControlRequest(void)
{
	HID_Device_ProcessControlRequest(&Keyboard_HID_Interface);
	HID_Device_ProcessControlRequest(&Mouse_HID_Interface);
}

/** Event handler for the USB device Start Of Frame event. */
void EVENT_USB_Device_StartOfFrame(void)
{
	HID_Device_MillisecondElapsed(&Keyboard_HID_Interface);
	HID_Device_MillisecondElapsed(&Mouse_HID_Interface);
	Update_Millis(1);
}

/** HID class driver callback function for the creation of HID reports to the host.
 *
 *  \param[in]     HIDInterfaceInfo  Pointer to the HID class interface configuration structure being referenced
 *  \param[in,out] ReportID    Report ID requested by the host if non-zero, otherwise callback should set to the generated report ID
 *  \param[in]     ReportType  Type of the report to create, either HID_REPORT_ITEM_In or HID_REPORT_ITEM_Feature
 *  \param[out]    ReportData  Pointer to a buffer where the created report should be stored
 *  \param[out]    ReportSize  Number of bytes written in the report (or zero if no report is to be sent
 *
 *  \return Boolean true to force the sending of the report, false to let the library determine if it needs to be sent
 */
bool CALLBACK_HID_Device_CreateHIDReport(USB_ClassInfo_HID_Device_t* const HIDInterfaceInfo, uint8_t* const ReportID,
										 const uint8_t ReportType, void* ReportData, uint16_t* const ReportSize)
{
	if (HIDInterfaceInfo == &Keyboard_HID_Interface){
		KeyboardReport_Data_t* KeyboardReport = (KeyboardReport_Data_t*)ReportData;

		*ReportSize = sizeof(KeyboardReport_Data_t);
		return Fill_KeyboardReport(KeyboardReport);

	}
	else{
		MouseReport_Data_t* MouseReport = (MouseReport_Data_t*)ReportData;
		*ReportSize = sizeof(MouseReport_Data_t);
		return Fill_MouseReport(MouseReport);
	}
	return false;
}


/** HID class driver callback function for the processing of HID reports from the host.
 *
 *  \param[in] HIDInterfaceInfo  Pointer to the HID class interface configuration structure being referenced
 *  \param[in] ReportID    Report ID of the received report from the host
 *  \param[in] ReportType  The type of report that the host has sent, either HID_REPORT_ITEM_Out or HID_REPORT_ITEM_Feature
 *  \param[in] ReportData  Pointer to a buffer where the created report has been stored
 *  \param[in] ReportSize  Size in bytes of the received HID report
 */
void CALLBACK_HID_Device_ProcessHIDReport(USB_ClassInfo_HID_Device_t* const HIDInterfaceInfo,
										  const uint8_t ReportID,
										  const uint8_t ReportType,
										  const void* ReportData,
										  const uint16_t ReportSize)
{
	if (HIDInterfaceInfo == &Keyboard_HID_Interface)
		{
			uint8_t* LEDReport = (uint8_t*)ReportData;
			Process_KeyboardLEDReport(*LEDReport);
		}
}
