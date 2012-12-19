#include <iostream>
#include <QDebug>
#include "keyboardpresenter.h"
#include "keyboardcomm.h"
#include "keyboardmodel.h"
#include "keyboardview.h"
#include "layout.h"

KeyboardPresenter::KeyboardPresenter()
	: mKeyboardModel(NULL)
{
	mView.reset(new KeyboardView(this, createSubviewList()));

	// distribute model to sub-presenters
	connect(this, SIGNAL(modelChanged(KeyboardModel*)),
			&mLayoutPresenter, SLOT(setModel(KeyboardModel*)));

	connect(this, SIGNAL(modelChanged(KeyboardModel*)),
			&mProgramsPresenter, SLOT(setModel(KeyboardModel*)));

	connect(this, SIGNAL(modelChanged(KeyboardModel*)),
			&mTriggersPresenter, SLOT(setModel(KeyboardModel*)));
}

QList<QPair<QString, QWidget*> > KeyboardPresenter::createSubviewList() {
	QList<QPair<QString, QWidget*> > subviews;
	subviews << QPair<QString, QWidget*>(
		tr("Layout"), mLayoutPresenter.getWidget());
	subviews << QPair<QString, QWidget*>(
		tr("Programs"), mProgramsPresenter.getWidget());
	subviews << QPair<QString, QWidget*>(
		tr("Triggers"), mTriggersPresenter.getWidget());
	return subviews;
}


// required to use a QScopedPointer with a forward decl (destructor
// must have full type available)
KeyboardPresenter::~KeyboardPresenter()
{
}

void KeyboardPresenter::showAction() {
	updateDeviceListAction();
	mView->show();
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
			qWarning() << "LIBUSBError during update: " << e.what();
		}
	}

	mView->updateDevices(names);
}

void KeyboardPresenter::selectDeviceAction(int index) {
	if (index == -1) {
		mView->showNoKeyboard();
		return;
	}
	else {
		mView->showKeyboard();
	}

	mUSBDevice.reset(new USBDevice(mDevices.at(index)));
	downloadAction();
}

void KeyboardPresenter::downloadAction() {
	if (!mUSBDevice) return;
	try {
		KeyboardComm comm(*mUSBDevice);

		KeyboardModel *m = new KeyboardModel(comm);
		mKeyboardModel.reset(m);
		emit modelChanged(m);
		mView->showValues(m->getLayoutID(),
						  m->getMappingSize(),
						  m->getNumPrograms(),
						  m->getProgramSpaceRaw(),
						  m->getProgramSpace(),
						  m->getMacroIndexSize(),
						  m->getMacroStorageSize());
	}
	catch (LIBUSBError& usbError) {
		qDebug() << "Error downloading settings: " << usbError.what();
	}
}

void hexDebug(const uint8_t* data, size_t len){
	char line[80];
	char* cursor = line;
	for(size_t i = 0; i < len; ++i){
		if(i && (i % 16 == 0)){
			qDebug() << line;
			cursor = line;
		}
		cursor += sprintf(cursor, "%.2x ", data[i]);
	}
	if(cursor > line)
		qDebug() << line;
}


void KeyboardPresenter::uploadAction() {
	if (!mUSBDevice) return;
	try {
		KeyboardComm comm(*mUSBDevice);

		comm.setMapping(mKeyboardModel->getMapping());

		comm.setPrograms(
			Program::encodePrograms(*mKeyboardModel->getPrograms(),
									mKeyboardModel->getNumPrograms(),
									mKeyboardModel->getProgramSpaceRaw()));

		QPair<QByteArray, QByteArray> encodedMacros =
			Trigger::encodeTriggers(*mKeyboardModel->getTriggers(),
									mKeyboardModel->getKeysPerTrigger(),
									mKeyboardModel->getMacroIndexSize(),
									mKeyboardModel->getMacroStorageSize());

		comm.setMacroIndex(encodedMacros.first);
		comm.setMacroStorage(encodedMacros.second);
	}
	catch (LIBUSBError& e) {
		qDebug() << "LIBUSBError setting mapping: " << e.what();
	}
}
