// -*- c++ -*-
#ifndef KEYBOARDPRESENTER_H
#define KEYBOARDPRESENTER_H

#include <QObject>
#include <QList>
#include <QScopedPointer>

#include "keyboardview.h"
#include "keyboardmodel.h"


class KeyboardPresenter : public QObject {
	Q_OBJECT

	QList<USBDevice> mDevices;
	KeyboardView mView;
	QScopedPointer<KeyboardModel> mKeyboardModel;

	KeyboardPresenter(const KeyboardPresenter& other);
	KeyboardPresenter& operator=(const KeyboardPresenter& other);
public:
	KeyboardPresenter()
		: mView(this)
		, mKeyboardModel(NULL)
	{}

public slots:
	void showAction();
	void selectDeviceAction(int index);
	void updateDeviceListAction();
};

#endif
