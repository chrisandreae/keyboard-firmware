#include <QList>
#include "keyboardcomm.h"

#include "devicemock.h"
#include "deviceusb.h"

KeyboardComm::DeviceList KeyboardComm::enumerate() {
	QList<QSharedPointer<Device> > keyboardDevices;
	DeviceUSB::enumerateTo(&keyboardDevices);
	#if defined(USE_MOCK)
		DeviceMock::enumerateTo(&keyboardDevices);
	#endif
	return keyboardDevices;
}
