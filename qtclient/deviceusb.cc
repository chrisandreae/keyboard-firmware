#include <exception>
#include <iostream>

#include <QDebug>
#include <QList>
#include <QByteArray>

#include <libusb.h>
#include "libusb_wrappers.h"

#include "keyboardcomm.h"
#include "deviceusb.h"

static const struct {
	uint16_t vid;
	uint16_t pid;
} valid_ids[] = {
	{0x16c0, 0x27db}, // VUSB IDs used by Kinesis modification
	{0x1d50, 0x6028}, // IDs used by Ergodox
	{0x16c0, 0x27dc}, // VUSB IDs used by ugboard
};

bool DeviceUSB::isValidID(const uint16_t vid, const uint16_t pid) {
	const int n_ids = sizeof(valid_ids) / sizeof(*valid_ids);
	for(int i = 0; i < n_ids; ++i) {
		if (vid == valid_ids[i].vid && pid == valid_ids[i].pid)
			return true;
	}
	return false;
}

QString DeviceUSB::getDeviceName(USBDevice* dev) {
	libusb_device_descriptor desc;
	LIBUSBCheckResult(
	    libusb_get_device_descriptor(*dev, &desc));

	USBDeviceHandle devHandle(*dev);
	char productBuf[256] = {0};
	LIBUSBCheckResult(
        libusb_get_string_descriptor_ascii(devHandle, desc.iProduct,
                                           (unsigned char*) productBuf,
                                           sizeof(productBuf) - 1));
	return QString(productBuf);
}

void DeviceUSB::enumerateTo(KeyboardComm::DeviceList* target, libusb_context* context) {
	libusb_device **deviceList = NULL;
	int cnt = LIBUSBCheckResult(
		libusb_get_device_list(context, &deviceList));

	for (int i = 0; i < cnt; i++) {
		libusb_device *dev = deviceList[i];
		try {
			libusb_device_descriptor desc;
			LIBUSBCheckResult(
			    libusb_get_device_descriptor(dev, &desc));

			if (!isValidID(desc.idVendor, desc.idProduct))
				continue;

			const unsigned char requiredPrefix[] = "andreae.gen.nz:";
			unsigned char buf[sizeof(requiredPrefix)] = {0};
			int length = LIBUSBCheckResult(
			    libusb_get_string_descriptor_ascii(
			        USBDeviceHandle(dev), desc.iSerialNumber, buf, sizeof(buf) - 1));

			if (strncmp(reinterpret_cast<const char*>(requiredPrefix),
			            reinterpret_cast<const char*>(buf),
			            length) != 0)
				continue;

			target->push_back(QSharedPointer<Device>{new DeviceUSB{dev}});
		} catch (LIBUSBError& e) {
			qWarning() << "LIBUSBError when enumerating: " << e.what();
		}
	}
	libusb_free_device_list(deviceList, 1);
}

void DeviceSessionUSB::doVendorRequest(uint8_t request, Direction dir,
                                char *buf, int bufLen,
                                uint16_t wValue, uint16_t wIndex)
{
	int returnSize =  LIBUSBCheckResult(
	    libusb_control_transfer(
	        mDeviceHandle,
	        LIBUSB_REQUEST_TYPE_VENDOR
	        | LIBUSB_RECIPIENT_DEVICE
	        | (dir == Read ? LIBUSB_ENDPOINT_IN : LIBUSB_ENDPOINT_OUT),
	        request,
	        wValue,
	        wIndex,
	        (unsigned char*) buf, bufLen,
	        mTimeout));
	if (returnSize != bufLen)
		throw DeviceError(DeviceError::Underflow);
}


QByteArray DeviceSessionUSB::getMapping() {
	QByteArray mapping(getMappingSize(), 0);
	doVendorRequest(READ_MAPPING, Read, mapping);
	return mapping;
}

void DeviceSessionUSB::setMapping(const QByteArray& mapping)
{
	doVendorRequest(WRITE_MAPPING, Write, const_cast<QByteArray&>(mapping));
}

QByteArray DeviceSessionUSB::getDefaultMapping() {
	QByteArray mapping(getMappingSize(), 0);
	doVendorRequest(READ_DEFAULT_MAPPING, Read, mapping);
	return mapping;
}

QByteArray DeviceSessionUSB::getPrograms() {
	QByteArray programs(getProgramSpaceRaw(), 0);
	doVendorRequest(READ_PROGRAMS, Read, programs);
	return programs;
}

void DeviceSessionUSB::setPrograms(const QByteArray& programs) {
	doVendorRequest(WRITE_PROGRAMS, Write, const_cast<QByteArray&>(programs));
}

QByteArray DeviceSessionUSB::getMacroIndex() {
	QByteArray macroIndex(getMacroIndexSize(), 0);
	doVendorRequest(READ_MACRO_INDEX, Read, macroIndex);
	return macroIndex;
}

void DeviceSessionUSB::setMacroIndex(const QByteArray& macroIndex) {
	doVendorRequest(WRITE_MACRO_INDEX, Write, const_cast<QByteArray&>(macroIndex));
}

QByteArray DeviceSessionUSB::getMacroStorage() {
	QByteArray macroStorage(getMacroStorageSize(), 0);
	doVendorRequest(READ_MACRO_STORAGE, Read, macroStorage);
	return macroStorage;
}

void DeviceSessionUSB::setMacroStorage(const QByteArray& macroStorage) {
	doVendorRequest(WRITE_MACRO_STORAGE, Write, const_cast<QByteArray&>(macroStorage));
}

void DeviceSessionUSB::reset() {
	doVendorRequest(RESET_DEFAULTS, Write, nullptr, 0);
}

void DeviceSessionUSB::resetFully() {
	doVendorRequest(RESET_FULLY, Write, nullptr, 0);
}
