// -*- c++ -*-
#ifndef KEYBOARDCOMM_H
#define KEYBOARDCOMM_H

#include <stdio.h>
#include <stdint.h>
#include <QList>
#include <QByteArray>
#include "libusb_wrappers.h"
#include "keyboard.h"

class KeyboardCommError : public std::exception {
public:
	enum Cause {
		Underflow,
	};

private:
	static const char *nameException(Cause);
	const Cause mCause;

public:
	KeyboardCommError(Cause c)
		: mCause(c)
	{}

	virtual const char *what() const throw() {
		return nameException(mCause);
	};
};

class InsufficentStorageException : public std::exception {
private:
	const int mBytes, mAvail;
	const char * const mStorageType;
	mutable QByteArray mMessage;

public:
	InsufficentStorageException(int bytes, int avail, const char* storageType)
		: mBytes(bytes)
		, mAvail(avail)
		, mStorageType(storageType)
	{}
	~InsufficentStorageException() throw () {
	}

	virtual const char* what() const throw() {
		if (mMessage.length() == 0) {
			mMessage = QString("Cannot store %1 bytes to %2: only %3 available.")
				.arg(mBytes)
				.arg(mStorageType)
				.arg(mAvail)
				.toAscii();
		}
		return mMessage.constData();
	}
};

class KeyboardComm {
	USBDeviceHandle mDeviceHandle;
	unsigned int mTimeout;

	enum Direction {
		Read, Write
	};

	template <typename T>
	T doSimpleVendorRequest(int8_t request, Direction dir,
                            uint16_t wValue = 0, uint16_t wIndex = 0)
	{
		T result;
		doVendorRequest(request, dir, (char*) &result, sizeof(result), wValue, wIndex);
		return result;
	}

	void doVendorRequest(uint8_t request, Direction dir,
	                    char *buf, int bufLen,
	                    uint16_t wValue = 0, uint16_t wIndex = 0);

	void doVendorRequest(uint8_t request, Direction dir,
	                    QByteArray& buf,
	                    uint16_t wValue = 0, uint16_t wIndex = 0)
	{
		return doVendorRequest(request, dir,
		                buf.data(), buf.length(),
		                wValue, wIndex);
	}

public:
	static QList<USBDevice> enumerate(libusb_context *context = NULL);

	KeyboardComm(const USBDevice& dev)
		: mDeviceHandle(dev)
		, mTimeout(5000)
	{
	}

	uint8_t getLayoutID() {
		return doSimpleVendorRequest<uint8_t>(READ_LAYOUT_ID, Read);
	}

	uint8_t getMappingSize() {
		return doSimpleVendorRequest<uint8_t>(READ_MAPPING_SIZE, Read);
	}

	uint8_t getNumPrograms() {
		return doSimpleVendorRequest<uint8_t>(READ_NUM_PROGRAMS, Read);
	}

	uint16_t getProgramSpaceRaw() {
		return doSimpleVendorRequest<uint16_t>(READ_PROGRAMS_SIZE, Read);
	}

	uint16_t getProgramSpace() {
		return getProgramSpaceRaw() - getNumPrograms() * 4;
	}

	uint16_t getMacroIndexSize() {
		return doSimpleVendorRequest<uint16_t>(READ_MACRO_INDEX_SIZE, Read);
	}

	uint16_t getMacroStorageSize() {
		return doSimpleVendorRequest<uint16_t>(READ_MACRO_STORAGE_SIZE, Read);
	}

	uint8_t getMacroMaxKeys() {
		return doSimpleVendorRequest<uint8_t>(READ_MACRO_MAX_KEYS, Read);
	}

	QByteArray getMapping();
	void setMapping(const QByteArray& mapping);
	QByteArray getDefaultMapping();

	QByteArray getPrograms();
	void setPrograms(const QByteArray& programs);

	QByteArray getMacroIndex();
	void setMacroIndex(const QByteArray& macroindex);

	QByteArray getMacroStorage();
	void setMacroStorage(const QByteArray& macroStorage);
};

#endif
