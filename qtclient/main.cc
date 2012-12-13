#include <QApplication>
#include "keyboardpresenter.h"
#include "libusb.h"

int main(int argc, char **argv) {
	QApplication app(argc, argv);

	libusb_init(NULL);

	KeyboardPresenter mainPresenter;
	mainPresenter.showAction();

	app.exec();
	libusb_exit(NULL);
}
