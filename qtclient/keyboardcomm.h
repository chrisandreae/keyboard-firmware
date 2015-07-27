// -*- c++ -*-
#ifndef KEYBOARDCOMM_H
#define KEYBOARDCOMM_H

#include <stdio.h>
#include <stdint.h>
#include <QList>
#include <QByteArray>
#include <QSharedPointer>

#include "libusb_wrappers.h"
#include "keyboard.h"
#include "device.h"


class KeyboardComm {
public:
	typedef QList<QSharedPointer<Device> > DeviceList;
	static DeviceList enumerate();
};

#endif
