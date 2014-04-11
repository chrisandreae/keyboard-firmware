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

#include "eeext_endpoint_stream.h"

#include "usb_vendor_interface.h"
#include "config.h"
#include "macro.h"
#include "macro_index.h"

/** LUFA HID Class driver interface configuration and state information. This structure is
 *  passed to all HID Class driver functions, so that multiple instances of the same class
 *  within a device can be differentiated from one another.
 */
USB_ClassInfo_HID_Device_t Keyboard_HID_Interface =
	{
		.Config =
			{
				.InterfaceNumber              = 1,

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
				.InterfaceNumber              = 2,

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

void USB_KeepAlive(uint8_t poll){
	static uint8_t in_task = 0;
	if(poll && !in_task){
		in_task = 1;
		USB_USBTask();
		in_task = 0;
	}
}

void USB_Perform_Update(uint8_t update_kbd, uint8_t update_mouse){
	if(update_mouse)
		HID_Device_USBTask(&Mouse_HID_Interface);

	if(update_kbd)
		HID_Device_USBTask(&Keyboard_HID_Interface);

	USB_KeepAlive(true);
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

	// TODO: Bounds check the transfers to make sure we don't overflow our
	// eeprom buffers.
	if (USB_ControlRequest.bmRequestType == (REQDIR_DEVICETOHOST | REQTYPE_VENDOR | REQREC_DEVICE)) {
		// Vendor message for us: accept the setup
		Endpoint_ClearSETUP();

		// Read requests
		switch(USB_ControlRequest.bRequest){
		case READ_NUM_PROGRAMS:
			Endpoint_Write_8(NUM_PROGRAMS);
			goto end_write;
		case READ_LAYOUT_ID:
			Endpoint_Write_8(LAYOUT_ID);
			goto end_write;
		case READ_MAPPING_SIZE:
			Endpoint_Write_8(NUM_LOGICAL_KEYS);
			goto end_write;
		case READ_CONFIG_FLAGS: {
			configuration_flags fs = config_get_flags();
			Endpoint_Write_8(*((uint8_t*)&fs));
			goto end_write;
		}
		case READ_MACRO_MAX_KEYS:
			Endpoint_Write_8(MACRO_MAX_KEYS);
			goto end_write;
		case READ_PROGRAMS_SIZE:
			Endpoint_Write_16_LE(PROGRAMS_SIZE);
			goto end_write;
		case READ_MACRO_INDEX_SIZE:
			Endpoint_Write_16_LE(MACRO_INDEX_SIZE);
			goto end_write;
		case READ_MACRO_STORAGE_SIZE:
			Endpoint_Write_16_LE(MACROS_SIZE);
		end_write:
			Endpoint_ClearIN(); // Finish sending to host
			Endpoint_ClearStatusStage(); // and wait for and clear the status ack
			break;
		case READ_PROGRAMS:
			Endpoint_Write_Control_SEStream_LE(config_get_programs(), USB_ControlRequest.wLength);
			goto ack_write_status;
		case READ_MACRO_INDEX:
			Endpoint_Write_Control_EStream_LE(macro_idx_get_storage(), USB_ControlRequest.wLength);
			goto ack_write_status;
		case READ_MACRO_STORAGE:
			Endpoint_Write_Control_SEStream_LE(macros_get_storage(), USB_ControlRequest.wLength);
			goto ack_write_status;
		case READ_DEFAULT_MAPPING:
			Endpoint_Write_Control_PStream_LE((uint8_t*)logical_to_hid_map_default, USB_ControlRequest.wLength);
			goto ack_write_status;
		case READ_MAPPING:
			Endpoint_Write_Control_EStream_LE(config_get_mapping(), USB_ControlRequest.wLength);
		ack_write_status:
			// Stream write functions already wait for the host's status ack, so we
			// just have to clear it.
			Endpoint_ClearOUT();
			break;

		default:
			// OK, we don't know what this request was, tell the host to reset us.
			// TODO: check that it's OK to call after clearing setup
			Endpoint_StallTransaction();
		}
	}
	else if (USB_ControlRequest.bmRequestType == (REQDIR_HOSTTODEVICE | REQTYPE_VENDOR | REQREC_DEVICE)) {
		// Write or message requests
		Endpoint_ClearSETUP();
		switch(USB_ControlRequest.bRequest){
			// write requests
		case WRITE_PROGRAMS:
			Endpoint_Read_Control_SEStream_LE(config_get_programs(), USB_ControlRequest.wLength);
			goto ack_read_status;
		case WRITE_MACRO_INDEX:
			Endpoint_Read_Control_EStream_LE(macro_idx_get_storage(), USB_ControlRequest.wLength);
			goto ack_read_status;
		case WRITE_MACRO_STORAGE:
			Endpoint_Read_Control_SEStream_LE(macros_get_storage(), USB_ControlRequest.wLength);
			goto ack_read_status;
		case WRITE_MAPPING:
			Endpoint_Read_Control_EStream_LE(config_get_mapping(), USB_ControlRequest.wLength);
		ack_read_status:
			// stream read functions already waited for the host to be ready:
			// just send the status ack
			Endpoint_ClearIN();
			break;
		// Message only requests with no data transfer: just need to ack
		case WRITE_CONFIG_FLAGS: {
			uint8_t flags = USB_ControlRequest.wValue & 0xff;
			config_save_flags(*(configuration_flags*)&flags);
			goto clear_status;
		}
		case RESET_DEFAULTS:
			config_reset_defaults();
			goto clear_status;
		case RESET_FULLY:
			config_reset_fully();
		clear_status:
			Endpoint_ClearStatusStage();
			break;
		default:
			Endpoint_StallTransaction();
		}
	}
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
		Fill_KeyboardReport(KeyboardReport);

	}
	else{
		MouseReport_Data_t* MouseReport = (MouseReport_Data_t*)ReportData;
		*ReportSize = sizeof(MouseReport_Data_t);
		Fill_MouseReport(MouseReport);
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
