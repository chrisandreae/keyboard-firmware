// -*- c++ -*-
#ifndef KEYBOARDCOMM_H
#define KEYBOARDCOMM_H

#include <stdint.h>
#include <QList>
#include <QByteArray>
#include "libusb_wrappers.h"
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
		doVendorRequest(request, dir, (char*) &result, sizeof(result), wValue, wIndex);
		return result;
	}

	void doVendorRequest(uint8_t request, Direction dir,
	                     char *buf, int bufLen,
	                     uint16_t wValue = 0, uint16_t wIndex = 0)
		throw (LIBUSBError);

	void doVendorRequest(uint8_t request, Direction dir,
						 QByteArray& buf,
	                     uint16_t wValue = 0, uint16_t wIndex = 0)
		throw (LIBUSBError)
	{
		doVendorRequest(request, dir,
						buf.data(), buf.length(),
						wValue, wIndex);
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

	QByteArray getMapping() throw (LIBUSBError);
	void setMapping(const QByteArray& mapping) throw (LIBUSBError);

	QByteArray getPrograms() throw (LIBUSBError);
	void setPrograms(const QByteArray& programs) throw (LIBUSBError);
};

#endif
