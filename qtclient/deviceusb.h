// -*- c++ -*-
#ifndef DEVICEUSB_H
#define DEVICEUSB_H

#include <libusb.h>
#include "libusb_wrappers.h"

#include "keyboard.h"
#include "device.h"

class DeviceSessionUSB : public DeviceSession {
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
	DeviceSessionUSB(const USBDevice& dev)
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

	void reset();
	void resetFully();
};

class DeviceUSB : public Device {
	USBDevice mDevice;
	QString mDeviceName;

	DeviceUSB(const USBDevice& dev)
		: mDevice(dev)
		, mDeviceName(getDeviceName(&mDevice))
	{
	}

	static bool isValidID(const uint16_t vid, const uint16_t pid);
	static QString getDeviceName(USBDevice* dev);

public:
	static void enumerateTo(KeyboardComm::DeviceList* target, libusb_context* context = NULL);

	virtual QSharedPointer<DeviceSession> newSession() override {
		return QSharedPointer<DeviceSession>(new DeviceSessionUSB(mDevice));
	}
	virtual QString getName() const override {
		return mDeviceName;
	}
};

#endif
