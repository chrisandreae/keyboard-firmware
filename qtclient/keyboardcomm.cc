#include <exception>
#include <iostream>

#include <QList>

#include <libusb.h>
#include "libusb_wrappers.h"

#include "keyboardcomm.h"

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
			// TODO: warning?
		}
	}
	libusb_free_device_list(deviceList, 1);
	return keyboardDevices;
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
