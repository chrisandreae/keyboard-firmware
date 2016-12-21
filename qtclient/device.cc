#include "device.h"

const char *DeviceError::nameException(DeviceError::Cause c) {
	switch (c) {
	case DeviceError::Underflow:
		return "Underflow";
	default:
		return "Unknown Error";
	}
}
