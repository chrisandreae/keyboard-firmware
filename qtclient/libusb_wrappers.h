// -*- c++ -*-
#ifndef LIBUSB_WRAPPERS_H
#define LIBUSB_WRAPPERS_H

#include <libusb.h>
#include <exception>

class LIBUSBError : public std::exception {
public:
	const libusb_error rawError;

	LIBUSBError(libusb_error error)
		: rawError(error)
	{}

	virtual const char *what() {
		return libusb_error_name(rawError);
	}
};

inline void LIBUSBHandleError(int ret) throw (LIBUSBError) {
	libusb_error rawError = (libusb_error) ret;
	throw LIBUSBError(rawError);
}

template <typename T>
inline T LIBUSBCheckResult(T value) throw (LIBUSBError) {
	if (value < 0)
		LIBUSBHandleError(value);
	return value;
}

class USBDevice {
	libusb_device *mDevice;

public:
	USBDevice()
	{
	}

	USBDevice(libusb_device *dev)
		: mDevice(dev)
	{
		libusb_ref_device(mDevice);
	}

	USBDevice& operator=(const USBDevice& other) {
		if (other.mDevice == mDevice)
			return *this;

		if (mDevice)
			libusb_unref_device(mDevice);

		mDevice = libusb_ref_device(other.mDevice);
	}

	virtual ~USBDevice() {
		if (mDevice)
			libusb_unref_device(mDevice);
	}

	USBDevice(const USBDevice& other)
		: mDevice(libusb_ref_device(other.mDevice))
	{}

	operator libusb_device*() {
		return mDevice;
	}
};

class USBDeviceHandle {
	libusb_device_handle *mDeviceHandle;

	USBDeviceHandle(libusb_device_handle *handle);

public:
	USBDeviceHandle(libusb_device *dev) throw (LIBUSBError)
	{
		LIBUSBCheckResult(libusb_open(dev, &mDeviceHandle));
	}

	virtual ~USBDeviceHandle() {
		libusb_close(mDeviceHandle);
	}

	operator libusb_device_handle*() {
		return mDeviceHandle;
	}
};

#endif
