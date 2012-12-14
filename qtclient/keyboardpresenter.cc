#include <iostream>
#include "keyboardpresenter.h"
#include "keyboardcomm.h"
#include "keyboardmodel.h"

void KeyboardPresenter::showAction() {
	updateDeviceListAction();
	mView.show();
}

void KeyboardPresenter::updateDeviceListAction() {
	QStringList names;

	mDevices = KeyboardComm::enumerate();
	QList<USBDevice> devices = mDevices;
	for (QList<USBDevice>::iterator it = devices.begin();
	     it != devices.end();
	     ++it)
	{
		try {
			libusb_device *dev = *it;
			libusb_device_descriptor desc;
			LIBUSBCheckResult(
			    libusb_get_device_descriptor(dev, &desc));

			USBDeviceHandle devHandle(dev);
			char productBuf[256] = {0};
			LIBUSBCheckResult(
			    libusb_get_string_descriptor_ascii(devHandle, desc.iProduct,
			                                       (unsigned char*) productBuf,
			                                       sizeof(productBuf) - 1));

			names.push_back(QString(productBuf));
		}
		catch (LIBUSBError& e) {
			// TODO: warning?
		}
	}

	mView.updateDevices(names);
}

void KeyboardPresenter::selectDeviceAction(int index) {
	if (index == -1) {
		mView.showNoKeyboard();
		return;
	}
	else
	{
		mView.showKeyboard();
	}

	USBDevice dev = mDevices.at(index);
	KeyboardComm comm(dev);
	KeyboardModel *m = new KeyboardModel(comm);
	mKeyboardModel.reset(m);
	mView.showValues(m->getLayoutID(),
					 m->getMappingSize(),
					 m->getNumPrograms(),
					 m->getProgramSpaceRaw(),
					 m->getProgramSpace(),
					 m->getMacroIndexSize(),
					 m->getMacroStorageSize());
}

