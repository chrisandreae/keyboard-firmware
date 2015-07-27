// -*- c++ -*-
#ifndef KEYBOARDPRESENTER_H
#define KEYBOARDPRESENTER_H

#include <QObject>
#include <QList>
#include <QScopedPointer>
#include <QSharedPointer>

#include "keyboardcomm.h"
#include "keyboardview.h"
#include "layoutpresenter.h"
#include "programspresenter.h"
#include "triggerspresenter.h"

class KeyboardModel;

class KeyboardComm;

class KeyboardPresenter : public QObject {
	Q_OBJECT
	Q_DISABLE_COPY(KeyboardPresenter)

	KeyboardComm::DeviceList mDevices;
	QSharedPointer<Device> mCurrentDevice;
	QScopedPointer<KeyboardView> mView;

	QSharedPointer<KeyboardModel> mKeyboardModel;

	LayoutPresenter mLayoutPresenter;
	ProgramsPresenter mProgramsPresenter;
	TriggersPresenter mTriggersPresenter;

	QList<QPair<QString, QWidget*> > createSubviewList();

public:
	KeyboardPresenter();
	~KeyboardPresenter();

signals:
	void modelChanged(const QSharedPointer<KeyboardModel>&);

public slots:
	void showAction();
	void selectDeviceAction(int index);
	void updateDeviceListAction();
	void uploadAction();
	void downloadAction();
};

#endif
