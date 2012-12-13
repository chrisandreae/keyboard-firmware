// -*- c++ -*-
#ifndef KEYBOARDPRESENTER_H
#define KEYBOARDPRESENTER_H

#include <QObject>
#include <QList>

#include "keyboardview.h"

class KeyboardModel;

class KeyboardPresenter : public QObject {
	Q_OBJECT

	QList<USBDevice> mDevices;
	KeyboardView mView;
	KeyboardModel *mKeyboardModel;

	void setKeyboardModel(KeyboardModel *model);


	KeyboardPresenter(const KeyboardPresenter& other);
	KeyboardPresenter& operator=(const KeyboardPresenter& other);
public:
	KeyboardPresenter()
		: mView(this)
		, mKeyboardModel(NULL)
	{}
	virtual ~KeyboardPresenter();

public slots:
	void showAction();
	void selectDeviceAction(int index);
	void updateDeviceListAction();
};

#endif
