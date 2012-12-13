TEMPLATE = app
TARGET = KeyboardClient
CONFIG += debug

HEADERS += keyboardview.h libusb_wrappers.h keyboard.h keyboardvalues.h keyboardcomm.h keyboardpresenter.h keyboardmodel.h
SOURCES +=  main.cc keyboardview.cc keyboardcomm.cc keyboardvalues.cc keyboardpresenter.cc keyboardmodel.cc

unix {
	CONFIG += link_pkgconfig
	PKGCONFIG += libusb-1.0
}
