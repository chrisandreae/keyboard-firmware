// -*- c++ -*-

#ifndef VALUESPRESENTER_H
#define VALUESPRESENTER_H

#include <QObject>
#include <QSharedPointer>

#include "device.h"
#include "keyboardmodel.h"
#include "keyboardvalues.h"

class ValuesPresenter : public QObject {
	Q_OBJECT

	KeyboardValues* mView;
	QSharedPointer<KeyboardModel> mModel;
	QSharedPointer<Device> mDevice;

public:
	ValuesPresenter();
	~ValuesPresenter();

	QWidget *getWidget() { return mView; }

public slots:
	void resetFully();
	void setModel(QSharedPointer<KeyboardModel> model);
	void setDevice(QSharedPointer<Device> device);

};

#endif // VALUESPRESENTER_H
