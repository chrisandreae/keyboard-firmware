/*
  Kinesis ergonomic keyboard firmware replacement

  Copyright 2012 Chris Andreae (chris (at) andreae.gen.nz)

  Licensed under the GNU GPL v2 (see GPL2.txt).

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

#include "Descriptors.h"
#include "hardware.h"

/** Mouse HID report descriptor. */
const USB_Descriptor_HIDReport_Datatype_t PROGMEM MouseReport[] =
	{
		HID_RI_USAGE_PAGE(8, 0x01), /* Generic Desktop */
		HID_RI_USAGE(8, 0x02), /* Mouse */
		HID_RI_COLLECTION(8, 0x01), /* Application */
		HID_RI_USAGE(8, 0x01), /* Pointer */
		HID_RI_COLLECTION(8, 0x00), /* Physical */
		HID_RI_USAGE_PAGE(8, 0x09), /* Button */
		HID_RI_USAGE_MINIMUM(8, 0x01),
		HID_RI_USAGE_MAXIMUM(8, 0x05),
		HID_RI_LOGICAL_MINIMUM(8, 0x00),
		HID_RI_LOGICAL_MAXIMUM(8, 0x01),
		HID_RI_REPORT_COUNT(8, 0x05),
		HID_RI_REPORT_SIZE(8, 0x01),
		HID_RI_INPUT(8, HID_IOF_DATA | HID_IOF_VARIABLE | HID_IOF_ABSOLUTE),
		HID_RI_REPORT_COUNT(8, 0x01),
		HID_RI_REPORT_SIZE(8, 0x03),
		HID_RI_INPUT(8, HID_IOF_CONSTANT),
		HID_RI_USAGE_PAGE(8, 0x01), /* Generic Desktop */
		HID_RI_USAGE(8, 0x30), /* Usage X */
		HID_RI_USAGE(8, 0x31), /* Usage Y */
		HID_RI_USAGE(8, 0x38), /* Usage wheel */
		HID_RI_LOGICAL_MINIMUM(8, -127),
		HID_RI_LOGICAL_MAXIMUM(8, 127),
		HID_RI_REPORT_COUNT(8, 0x03),
		HID_RI_REPORT_SIZE(8, 0x08),
		HID_RI_INPUT(8, HID_IOF_DATA | HID_IOF_VARIABLE | HID_IOF_RELATIVE),
		HID_RI_END_COLLECTION(0),
		HID_RI_END_COLLECTION(0),
	};

/** Keyboard HID report descriptor. */
const USB_Descriptor_HIDReport_Datatype_t PROGMEM KeyboardReport[] =
	{
		HID_RI_USAGE_PAGE(8, 0x01), /* Generic Desktop */
		HID_RI_USAGE(8, 0x06), /* Keyboard */
		HID_RI_COLLECTION(8, 0x01), /* Application */
		HID_RI_USAGE_PAGE(8, 0x07), /* Key Codes */
		HID_RI_USAGE_MINIMUM(8, 0xE0), /* Keyboard Left Control */
		HID_RI_USAGE_MAXIMUM(8, 0xE7), /* Keyboard Right GUI */
		HID_RI_LOGICAL_MINIMUM(8, 0x00),
		HID_RI_LOGICAL_MAXIMUM(8, 0x01),
		HID_RI_REPORT_SIZE(8, 0x01),
		HID_RI_REPORT_COUNT(8, 0x08),
		HID_RI_INPUT(8, HID_IOF_DATA | HID_IOF_VARIABLE | HID_IOF_ABSOLUTE),
		HID_RI_REPORT_COUNT(8, 0x01),
		HID_RI_REPORT_SIZE(8, 0x08),
		HID_RI_INPUT(8, HID_IOF_CONSTANT),
		HID_RI_USAGE_PAGE(8, 0x08), /* LEDs */
		HID_RI_USAGE_MINIMUM(8, 0x01), /* Num Lock */
		HID_RI_USAGE_MAXIMUM(8, 0x05), /* Kana */
		HID_RI_REPORT_COUNT(8, 0x05),
		HID_RI_REPORT_SIZE(8, 0x01),
		HID_RI_OUTPUT(8, HID_IOF_DATA | HID_IOF_VARIABLE | HID_IOF_ABSOLUTE | HID_IOF_NON_VOLATILE),
		HID_RI_REPORT_COUNT(8, 0x01),
		HID_RI_REPORT_SIZE(8, 0x03),
		HID_RI_OUTPUT(8, HID_IOF_CONSTANT),
		HID_RI_LOGICAL_MINIMUM(8, 0x00),
		HID_RI_LOGICAL_MAXIMUM(8, 0x65),
		HID_RI_USAGE_PAGE(8, 0x07), /* Keyboard */
		HID_RI_USAGE_MINIMUM(8, 0x00), /* Reserved (no event indicated) */
		HID_RI_USAGE_MAXIMUM(8, 0x65), /* Keyboard Application */
		HID_RI_REPORT_COUNT(8, KEYBOARDREPORT_KEY_COUNT),
		HID_RI_REPORT_SIZE(8, 0x08),
		HID_RI_INPUT(8, HID_IOF_DATA | HID_IOF_ARRAY | HID_IOF_ABSOLUTE),
		HID_RI_END_COLLECTION(0),
	};

#ifdef BUILD_FOR_LUFA
/** Device descriptor structure. This descriptor, located in FLASH memory, describes the overall
 *  device characteristics, including the supported USB version, control endpoint size and the
 *  number of device configurations. The descriptor is read out by the USB host when the enumeration
 *  process begins.
 */
const USB_Descriptor_Device_t PROGMEM DeviceDescriptor =
{
	.Header                 = {.Size = sizeof(USB_Descriptor_Device_t), .Type = DTYPE_Device},

	.USBSpecification       = VERSION_BCD(01.10),
	.Class                  = USB_CSCP_NoDeviceClass,
	.SubClass               = USB_CSCP_NoDeviceSubclass,
	.Protocol               = USB_CSCP_NoDeviceProtocol,

	.Endpoint0Size          = FIXED_CONTROL_ENDPOINT_SIZE,

	.VendorID               = USB_VENDOR_ID,  // Defined in hardware.h
	.ProductID              = USB_PRODUCT_ID,
	.ReleaseNumber          = VERSION_BCD(01.10),

	.ManufacturerStrIndex   = 0x01,
	.ProductStrIndex        = 0x02,
	.SerialNumStrIndex      = 0x03,

	.NumberOfConfigurations = FIXED_NUM_CONFIGURATIONS
};

#endif

const USB_Descriptor_Configuration_t PROGMEM ConfigurationDescriptor =
{
	.Config =
	{
		.Header                 = {.Size = sizeof(USB_Descriptor_Configuration_Header_t), .Type = DTYPE_Configuration},

		.TotalConfigurationSize = sizeof(USB_Descriptor_Configuration_t),
		.TotalInterfaces        = 2,

		.ConfigurationNumber    = 1,
		.ConfigurationStrIndex  = NO_DESCRIPTOR,

		.ConfigAttributes       = (USB_CONFIG_ATTR_BUSPOWERED | USB_CONFIG_ATTR_SELFPOWERED),

		.MaxPowerConsumption    = USB_CONFIG_POWER_MA(30)
	},

	.HID1_KeyboardInterface =
	{
		.Header                 = {.Size = sizeof(USB_Descriptor_Interface_t), .Type = DTYPE_Interface},

		.InterfaceNumber        = 0x00,
		.AlternateSetting       = 0x00,

		.TotalEndpoints         = 1,

		.Class                  = HID_CSCP_HIDClass,
		.SubClass               = HID_CSCP_BootSubclass,
		.Protocol               = HID_CSCP_KeyboardBootProtocol,

		.InterfaceStrIndex      = NO_DESCRIPTOR
	},

	.HID1_KeyboardHID =
	{
		.Header                 = {.Size = sizeof(USB_HID_Descriptor_HID_t), .Type = HID_DTYPE_HID},

		.HIDSpec                = VERSION_BCD(01.11),
		.CountryCode            = 0x00,
		.TotalReportDescriptors = 1,
		.HIDReportType          = HID_DTYPE_Report,
		.HIDReportLength        = sizeof(KeyboardReport)
	},

	.HID1_ReportINEndpoint =
	{
		.Header                 = {.Size = sizeof(USB_Descriptor_Endpoint_t), .Type = DTYPE_Endpoint},

		.EndpointAddress        = (ENDPOINT_DESCRIPTOR_DIR_IN | KEYBOARD_IN_EPNUM),
		.Attributes             = (EP_TYPE_INTERRUPT | ENDPOINT_ATTR_NO_SYNC | ENDPOINT_USAGE_DATA),
		.EndpointSize           = HID_EPSIZE,
		.PollingIntervalMS      = 0x01
	},

	.HID2_MouseInterface =
	{
		.Header                 = {.Size = sizeof(USB_Descriptor_Interface_t), .Type = DTYPE_Interface},

		.InterfaceNumber        = 0x01,
		.AlternateSetting       = 0x00,

		.TotalEndpoints         = 1,

		.Class                  = HID_CSCP_HIDClass,
		.SubClass               = HID_CSCP_BootSubclass,
		.Protocol               = HID_CSCP_MouseBootProtocol,

		.InterfaceStrIndex      = NO_DESCRIPTOR
	},

	.HID2_MouseHID =
	{
		.Header                 = {.Size = sizeof(USB_HID_Descriptor_HID_t), .Type = HID_DTYPE_HID},

		.HIDSpec                = VERSION_BCD(01.11),
		.CountryCode            = 0x00,
		.TotalReportDescriptors = 1,
		.HIDReportType          = HID_DTYPE_Report,
		.HIDReportLength        = sizeof(MouseReport)
	},

	.HID2_ReportINEndpoint =
	{
		.Header                 = {.Size = sizeof(USB_Descriptor_Endpoint_t), .Type = DTYPE_Endpoint},

		.EndpointAddress        = (ENDPOINT_DESCRIPTOR_DIR_IN | MOUSE_IN_EPNUM),
		.Attributes             = (EP_TYPE_INTERRUPT | ENDPOINT_ATTR_NO_SYNC | ENDPOINT_USAGE_DATA),
		.EndpointSize           = HID_EPSIZE,
		.PollingIntervalMS      = 0x01
	}

};

#ifdef BUILD_FOR_LUFA
#define USB_STRING_LEN_OF(x) (sizeof(USB_Descriptor_Header_t) + sizeof(x) - 2)

/** Language descriptor structure. This descriptor, located in FLASH memory, is returned when the host requests
 *  the string descriptor with index 0 (the first index). It is actually an array of 16-bit integers, which indicate
 *  via the language ID table available at USB.org what languages the device supports for its string descriptors.
 */
const USB_Descriptor_String_t PROGMEM LanguageString =
{
	.Header                 = {.Size = USB_STRING_LEN(1), .Type = DTYPE_String},

	.UnicodeString          = {LANGUAGE_ID_ENG}
};

/** Manufacturer descriptor string. This is a Unicode string containing the manufacturer's details in human readable
 *  form, and is read out upon request by the host when the appropriate string ID is requested, listed in the Device
 *  Descriptor.
 */
const USB_Descriptor_String_t PROGMEM ManufacturerString =
{
	.Header                 = {.Size = USB_STRING_LEN_OF(USB_MANUFACTURER_STRING), .Type = DTYPE_String},

	.UnicodeString          = USB_MANUFACTURER_STRING
};

/** Product descriptor string. This is a Unicode string containing the product's details in human readable form,
 *  and is read out upon request by the host when the appropriate string ID is requested, listed in the Device
 *  Descriptor.
 */
const USB_Descriptor_String_t PROGMEM ProductString =
{
	.Header                 = {.Size = USB_STRING_LEN_OF(USB_PRODUCT_STRING), .Type = DTYPE_String},

	.UnicodeString          = USB_PRODUCT_STRING
};

const USB_Descriptor_String_t PROGMEM SerialNumberString =
{
	.Header                 = {.Size = USB_STRING_LEN_OF(USB_SERIAL_NUMBER_STRING), .Type = DTYPE_String},

	.UnicodeString          = USB_SERIAL_NUMBER_STRING
};

#endif

#ifdef BUILD_FOR_LUFA
/** This function is called by the library when in device mode, and must be overridden (see library "USB Descriptors"
 *  documentation) by the application code so that the address and size of a requested descriptor can be given
 *  to the USB library. When the device receives a Get Descriptor request on the control endpoint, this function
 *  is called so that the descriptor details can be passed back and the appropriate descriptor sent back to the
 *  USB host.
 */
uint16_t CALLBACK_USB_GetDescriptor(const uint16_t wValue,
									const uint8_t wIndex,
									const void** const DescriptorAddress)
{
	const uint8_t  DescriptorType   = (wValue >> 8);
	const uint8_t  DescriptorNumber = (wValue & 0xFF);

	const void* Address = NULL;
	uint16_t    Size    = NO_DESCRIPTOR;

	switch (DescriptorType)
	{
		case DTYPE_Device:
			Address = &DeviceDescriptor;
			Size    = sizeof(USB_Descriptor_Device_t);
			break;
		case DTYPE_Configuration:
			Address = &ConfigurationDescriptor;
			Size    = sizeof(USB_Descriptor_Configuration_t);
			break;
		case DTYPE_String:
			switch (DescriptorNumber)
			{
				case 0x00:
					Address = &LanguageString;
					Size    = pgm_read_byte(&LanguageString.Header.Size);
					break;
				case 0x01:
					Address = &ManufacturerString;
					Size    = pgm_read_byte(&ManufacturerString.Header.Size);
					break;
				case 0x02:
					Address = &ProductString;
					Size    = pgm_read_byte(&ProductString.Header.Size);
					break;
				case 0x03:
					Address = &SerialNumberString;
					Size    = pgm_read_byte(&SerialNumberString.Header.Size);
					break;

			}

			break;
		case HID_DTYPE_HID:
			if (!(wIndex))
			{
				Address = &ConfigurationDescriptor.HID1_KeyboardHID;
				Size    = sizeof(USB_HID_Descriptor_HID_t);
			}
			else
			{
				Address = &ConfigurationDescriptor.HID2_MouseHID;
				Size    = sizeof(USB_HID_Descriptor_HID_t);
			}
			break;
		case HID_DTYPE_Report:
			if (!(wIndex))
			{
				Address = &KeyboardReport;
				Size    = sizeof(KeyboardReport);
			}
			else
			{
				Address = &MouseReport;
				Size    = sizeof(MouseReport);
			}

			break;
	}

	*DescriptorAddress = Address;
	return Size;
}
#endif

#ifdef BUILD_FOR_VUSB

#include "usbdrv.h"
usbMsgLen_t usbFunctionDescriptor(usbRequest_t* rq)
{

	const uint8_t  DescriptorType   = (rq->wValue.word >> 8);
#if 0
	const uint8_t  DescriptorNumber = (rq->wValue.word & 0xFF);
#endif

	const void* Address = 0;
	uint16_t    Size    = 0;

	switch (DescriptorType)
		{
#if DEFINE_DEVICE
		case DTYPE_Device:
			Address = &DeviceDescriptor;
			Size    = sizeof(USB_Descriptor_Device_t);
			break;
#endif
		case DTYPE_Configuration:
			Address = &ConfigurationDescriptor;
			Size    = sizeof(USB_Descriptor_Configuration_t);
			break;
#if 0
		case DTYPE_String:
			switch (DescriptorNumber)
				{
				case 0x00:
					Address = &LanguageString;
					Size    = pgm_read_byte(&LanguageString.Header.Size);
					break;
				case 0x01:
					Address = &ManufacturerString;
					Size    = pgm_read_byte(&ManufacturerString.Header.Size);
					break;
				case 0x02:
					Address = &ProductString;
					Size    = pgm_read_byte(&ProductString.Header.Size);
					break;
				case 0x03:
					Address = &SerialNumberString;
					Size    = pgm_read_byte(&SerialNumberString.Header.Size);
					break;
				}
#endif

			break;
		case HID_DTYPE_HID:
			if (!(rq->wIndex.word))
				{
					Address = &ConfigurationDescriptor.HID1_KeyboardHID;
					Size    = sizeof(USB_HID_Descriptor_HID_t);
				}
			else
				{
					Address = &ConfigurationDescriptor.HID2_MouseHID;
					Size    = sizeof(USB_HID_Descriptor_HID_t);
				}
			break;
		case HID_DTYPE_Report:
			if (!(rq->wIndex.word))
				{
					Address = &KeyboardReport;
					Size    = sizeof(KeyboardReport);
				}
			else
				{
					Address = &MouseReport;
					Size    = sizeof(MouseReport);
				}

			break;
		}

	usbMsgPtr = (void*) Address;
	return Size;
}


#endif
