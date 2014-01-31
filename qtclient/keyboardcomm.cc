#include <exception>
#include <iostream>

#include <QDebug>
#include <QList>
#include <QByteArray>

#include <libusb.h>
#include "libusb_wrappers.h"

#include "keyboardcomm.h"

const char *KeyboardCommError::nameException(KeyboardCommError::Cause c) {
	switch (c) {
	case KeyboardCommError::Underflow:
		return "Underflow";
	default:
		return "Unknown Error";
	}
}

QList<USBDevice> KeyboardComm::enumerate(libusb_context *context) {
	libusb_device **deviceList = NULL;
	int cnt = LIBUSBCheckResult(
		libusb_get_device_list(context, &deviceList));

	QList<USBDevice> keyboardDevices;

	for (int i = 0; i < cnt; i++) {
		libusb_device *dev = deviceList[i];
		try {
			libusb_device_descriptor desc;
			LIBUSBCheckResult(
			    libusb_get_device_descriptor(dev, &desc));

			if (desc.idVendor != 0x16c0 || desc.idProduct != 0x27db)
				continue;

			USBDeviceHandle d(dev);
			const unsigned char requiredPrefix[] = "andreae.gen.nz";
			unsigned char buf[sizeof(requiredPrefix)] = {0};
			int length = LIBUSBCheckResult(
			    libusb_get_string_descriptor_ascii(d, desc.iSerialNumber, buf, sizeof(buf) - 1));

			if (strncmp(reinterpret_cast<const char*>(requiredPrefix),
			            reinterpret_cast<const char*>(buf),
			            length) != 0)
				continue;

			keyboardDevices.push_back(USBDevice(dev));
		} catch (LIBUSBError& e) {
			qWarning() << "LIBUSBError when enumerating: " << e.what();
		}
	}
	libusb_free_device_list(deviceList, 1);
	return keyboardDevices;
}

void KeyboardComm::doVendorRequest(uint8_t request, Direction dir,
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
		throw KeyboardCommError(KeyboardCommError::Underflow);
}


QByteArray KeyboardComm::getMapping() {
	QByteArray mapping(getMappingSize(), 0);
	doVendorRequest(READ_MAPPING, Read, mapping);
	return mapping;
}

void KeyboardComm::setMapping(const QByteArray& mapping)
{
	doVendorRequest(WRITE_MAPPING, Write, const_cast<QByteArray&>(mapping));
}

QByteArray KeyboardComm::getDefaultMapping() {
	QByteArray mapping(getMappingSize(), 0);
	doVendorRequest(READ_DEFAULT_MAPPING, Read, mapping);
	return mapping;
}

QByteArray KeyboardComm::getPrograms() {
	QByteArray programs(getProgramSpaceRaw(), 0);
	doVendorRequest(READ_PROGRAMS, Read, programs);
	return programs;
}

void KeyboardComm::setPrograms(const QByteArray& programs) {
	doVendorRequest(WRITE_PROGRAMS, Write, const_cast<QByteArray&>(programs));
}

QByteArray KeyboardComm::getMacroIndex() {
	QByteArray macroIndex(getMacroIndexSize(), 0);
	doVendorRequest(READ_MACRO_INDEX, Read, macroIndex);
	return macroIndex;
}

void KeyboardComm::setMacroIndex(const QByteArray& macroIndex) {
	doVendorRequest(WRITE_MACRO_INDEX, Write, const_cast<QByteArray&>(macroIndex));
}

QByteArray KeyboardComm::getMacroStorage() {
	QByteArray macroStorage(getMacroStorageSize(), 0);
	doVendorRequest(READ_MACRO_STORAGE, Read, macroStorage);
	return macroStorage;
}

void KeyboardComm::setMacroStorage(const QByteArray& macroStorage) {
	doVendorRequest(WRITE_MACRO_STORAGE, Write, const_cast<QByteArray&>(macroStorage));
}

#if 0
int main(int argc, char **argv) {
	libusb_init(NULL);
	QList<USBDevice> devices = KeyboardComm::enumerate();

	for (QList<USBDevice>::iterator it = devices.begin();
		 it != devices.end();
		 ++it)
	{
		std::cout << "device!" << std::endl;
		try {
			USBDeviceHandle d(*it);
			unsigned char serial[256] = {0};
			int length = LIBUSBCheckResult(
				libusb_get_string_descriptor_ascii(d, 1, serial, sizeof(serial) - 1));
			KeyboardComm comm(*it);
			std::cout << "\tSerial = " << serial << std::endl
					  << "\tLayoutID = " << (int)comm.getLayoutID() << std::endl
					  << "\tMappingSize = " << (int)comm.getMappingSize() << std::endl
					  << "\tNumPrograms = " << (int)comm.getNumPrograms() << std::endl
					  << "\tProgramSpace (RAW) = " << (int)comm.getProgramSpaceRaw() << std::endl
					  << "\tProgramSpace = " << (int)comm.getProgramSpace() << std::endl
					  << "\tMacroIndexSize = " << (int)comm.getMacroIndexSize() << std::endl
					  << "\tMacroStorageSize = " << (int)comm.getMacroStorageSize() << std::endl;

		}
		catch (LIBUSBError& e) {
			std::cout << "Error listing devices: " << e.what() << std::endl;
		}
	}
	libusb_exit(NULL);
}
#endif
