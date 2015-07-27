#include <QDebug>

#include "valuespresenter.h"

#include "device.h"
#include "keyboardvalues.h"
#include "keyboardmodel.h"


ValuesPresenter::ValuesPresenter() {
	mView = new KeyboardValues(this);
}

ValuesPresenter::~ValuesPresenter() {
	if (!mView->parent()) {
		delete mView;
	}
}

void ValuesPresenter::setModel(QSharedPointer<KeyboardModel> model) {
	mModel = model;
	mView->showValues(mModel->getLayoutID(),
	                  mModel->getMappingSize(),
	                  mModel->getNumPrograms(),
	                  mModel->getProgramSpaceRaw(),
	                  mModel->getProgramSpace(),
	                  mModel->getMacroIndexSize(),
	                  mModel->getMacroStorageSize());
}

void ValuesPresenter::setDevice(QSharedPointer<Device> device) {
	mDevice = device;
}

void ValuesPresenter::resetFully() {
	if (!mDevice)
		return;

	try {
		QSharedPointer<DeviceSession> session =
		    mDevice->newSession();
		session->resetFully();
	}
	catch (DeviceError& e) {
		qDebug() << "DeviceError resetting: " << e.what();
	}
}
