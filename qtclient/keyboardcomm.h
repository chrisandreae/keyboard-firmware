// -*- c++ -*-
#ifndef KEYBOARDCOMM_H
#define KEYBOARDCOMM_H

#include "keyboard.h"

class KeyboardComm {
	USBDeviceHandle mDeviceHandle;
	unsigned int mTimeout;

	enum Direction {
		Read, Write
	};

	template <typename T>
	int doSimpleVendorRequest(uint8_t request, Direction dir,
	                          uint16_t wValue = 0, uint16_t wIndex = 0)
		throw (LIBUSBError)
	{
		T result;
		LIBUSBCheckResult(
		    libusb_control_transfer(
		        mDeviceHandle,
		        LIBUSB_REQUEST_TYPE_VENDOR
		        | LIBUSB_RECIPIENT_DEVICE
		        | (dir == Read ? LIBUSB_ENDPOINT_IN : LIBUSB_ENDPOINT_OUT),
		        request,
		        wValue,
		        wIndex,
		        (unsigned char*) &result, sizeof(result),
		        mTimeout));
		return result;
	}

public:
	static QList<USBDevice> enumerate(libusb_context *context = NULL);

	KeyboardComm(USBDevice& dev)
		: mDeviceHandle(dev)
		, mTimeout(5000)
	{
	}

	uint8_t getLayoutID() throw (LIBUSBError) {
		return doSimpleVendorRequest<uint8_t>(READ_LAYOUT_ID, Read);
	}

	uint8_t getMappingSize() throw (LIBUSBError) {
		return doSimpleVendorRequest<uint8_t>(READ_MAPPING_SIZE, Read);
	}

	uint8_t getNumPrograms() throw (LIBUSBError) {
		return doSimpleVendorRequest<uint8_t>(READ_NUM_PROGRAMS, Read);
	}

	uint16_t getProgramSpaceRaw() throw (LIBUSBError) {
		return doSimpleVendorRequest<uint16_t>(READ_PROGRAMS_SIZE, Read);
	}

	uint16_t getProgramSpace() throw (LIBUSBError) {
		return getProgramSpaceRaw() - getNumPrograms() * 4;
	}

	uint16_t getMacroIndexSize() throw (LIBUSBError) {
		return doSimpleVendorRequest<uint16_t>(READ_MACRO_INDEX_SIZE, Read);
	}

	uint16_t getMacroStorageSize() throw (LIBUSBError) {
		return doSimpleVendorRequest<uint16_t>(READ_MACRO_STORAGE_SIZE, Read);
	}
};

#endif
