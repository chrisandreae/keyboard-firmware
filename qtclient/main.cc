#include <QApplication>
#include "mainwindow.h"
#include "libusb.h"

int main(int argc, char **argv) {
	QApplication app(argc, argv);

	libusb_init(NULL);
	MainWindow win;
	win.show();
	app.exec();
	libusb_exit(NULL);
}
