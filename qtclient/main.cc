#include <QApplication>
#include "keyboardpresenter.h"
#include "libusb.h"

#ifdef USE_COMPILER
#include <HsFFI.h>
#endif

int main(int argc, char **argv) {
	#ifdef USE_COMPILER
		hs_init(&argc, &argv);
	#endif

	QApplication app(argc, argv);

	libusb_init(NULL);

	{
		KeyboardPresenter mainPresenter;
		mainPresenter.showAction();
		app.exec();
	}

	libusb_exit(NULL);
	#ifdef USE_COMPILER
		hs_exit();
	#endif
}
