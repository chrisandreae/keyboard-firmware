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
	layoutpresenter.h \
	layoutview.h \
	libusb_wrappers.h \

SOURCES += \
	keyboardcomm.cc \
	keyboardmodel.cc \
	keyboardpresenter.cc \
	keyboardvalues.cc \
	keyboardview.cc \
	layoutpresenter.cc \
	layoutview.cc \
	main.cc \

unix {
	CONFIG += link_pkgconfig
	PKGCONFIG += libusb-1.0
}
