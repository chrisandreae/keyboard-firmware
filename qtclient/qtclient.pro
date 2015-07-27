# -*- Makefile -*-

TEMPLATE = app
TARGET = KeyboardClient
CONFIG += debug
QT += xml
DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x000000

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

QMAKE_CXXFLAGS += -std=c++11

RESOURCES += qtclient.rsc

PRECOMPILED_HEADER = prefix.h

HEADERS += \
	keyboard.h \
	keyboardcomm.h \
	keyboardmodel.h \
	keyboardpresenter.h \
	keyboardvalues.h \
	valuespresenter.h \
	keyboardview.h \
	layout.h \
	layoutpresenter.h \
	layoutview.h \
	libusb_wrappers.h \
	hidtables.h \
	keyselectionview.h \
	hidusageproxymodel.h \
	program.h \
	programsview.h \
	programsitemmodel.h \
	programspresenter.h \
	vm.h \
	layoutwidget.h \
	layeredlayoutwidget.h \
	triggersview.h \
	triggerspresenter.h \
	triggersitemmodel.h \
	trigger.h \
	hexdump.h \
	triggersitemdelegate.h \
	util.h \
	device.h \
	deviceusb.h \
	devicemock.h \

SOURCES += \
	keyboardcomm.cc \
	keyboardmodel.cc \
	keyboardpresenter.cc \
	keyboardvalues.cc \
	valuespresenter.cc \
	keyboardview.cc \
	layoutpresenter.cc \
	layoutview.cc \
	main.cc \
	hidtables.cc \
	keyselectionview.cc \
	hidusageproxymodel.cc \
	program.cc \
	programsview.cc \
	programsitemmodel.cc \
	programspresenter.cc \
	layoutwidget.cc \
	layeredlayoutwidget.cc \
	triggersview.cc \
	triggerspresenter.cc \
	triggersitemmodel.cc \
	trigger.cc \
	layout.cc \
	hexdump.cc \
	triggersitemdelegate.cc \
	util.cc \
	device.cc \
	deviceusb.cc \
	devicemock.cc \

mac {
	CONFIG += link_pkgconfig
	PKGCONFIG += libusb-1.0
}

linux-* {
	CONFIG += link_pkgconfig
	PKGCONFIG += libusb-1.0
}

freebsd-* {
	LIBS += -lusb
}

win32 {
	INCLUDEPATH += c:\\lib\\libusbx-1.0.14-win\\include\\libusbx-1.0
	LIBS += -Lc:\\lib\\libusbx-1.0.14-win\\MS32\\static -llibusb-1.0
}

contains(USE_MOCK, 1) {
	DEFINES += USE_MOCK
}
