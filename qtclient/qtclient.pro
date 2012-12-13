TEMPLATE = app
TARGET = KeyboardClient
CONFIG += debug

HEADERS += mainwindow.h libusb_wrappers.h keyboard.h keyboardvalues.h keyboardcomm.h
SOURCES +=  main.cc mainwindow.cc keyboardcomm.cc keyboardvalues.cc

unix {
	CONFIG += link_pkgconfig
	PKGCONFIG += libusb-1.0
}
