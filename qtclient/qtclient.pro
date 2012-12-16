# -*- Makefile -*-

TEMPLATE = app
TARGET = KeyboardClient
CONFIG += debug
QT += xml

QMAKE_BUNDLE_DATA += layout

macx {
	layout.path = Resources/Layout
	layout.files = layout/1.xml layout/kinesis.png
}

HEADERS += \
	keyboard.h \
	keyboardcomm.h \
	keyboardmodel.h \
	keyboardpresenter.h \
	keyboardvalues.h \
	keyboardview.h \
	layout.h \
	layoutpresenter.h \
	layoutview.h \
	libusb_wrappers.h \
	hidtables.h \
	mapping.h \
	keyselectionview.h \
	hidusageproxymodel.h \
	program.h \
	programsview.h \
	programsitemmodel.h \
	programspresenter.h \

SOURCES += \
	keyboardcomm.cc \
	keyboardmodel.cc \
	keyboardpresenter.cc \
	keyboardvalues.cc \
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

unix {
	CONFIG += link_pkgconfig
	PKGCONFIG += libusb-1.0
}
