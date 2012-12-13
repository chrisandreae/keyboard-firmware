#include <iostream>

#include <Qt>
#include <QToolBar>
#include <QComboBox>
#include <QLineEdit>

#include "mainwindow.h"
#include "keyboardcomm.h"
#include "keyboardvalues.h"

enum MyRoles {
	USBDeviceRole = Qt::UserRole + 1,
};

MainWindow::MainWindow()
{
	mKeyboardsModel = createKeyboardItemModel();

	QToolBar *toolBar = addToolBar(tr("Keyboard Selection"));
	QComboBox *keyboardSelection = new QComboBox;
	keyboardSelection->setModel(mKeyboardsModel);
	toolBar->addWidget(keyboardSelection);

	connect(keyboardSelection, SIGNAL(currentIndexChanged(int)),
	        this, SLOT(keyboardSelected(int)));

	setCentralWidget(mKeyboardValues = new KeyboardValues);

	setUnifiedTitleAndToolBarOnMac(true);

}

QStandardItemModel *MainWindow::createKeyboardItemModel() {
	QStandardItemModel *model = new QStandardItemModel;
	QStandardItem *parent = model->invisibleRootItem();

	QList<USBDevice> devices = KeyboardComm::enumerate();
	for (QList<USBDevice>::iterator it = devices.begin();
	     it != devices.end();
	     ++it)
	{
		try {
			libusb_device *dev = *it;
			libusb_device_descriptor desc;
			LIBUSBCheckResult(
			    libusb_get_device_descriptor(dev, &desc));

			USBDeviceHandle devHandle(dev);
			char productBuf[256] = {0};
			LIBUSBCheckResult(
			    libusb_get_string_descriptor_ascii(devHandle, desc.iProduct,
			                                       (unsigned char*) productBuf,
			                                       sizeof(productBuf) - 1));

			QStandardItem *item = new QStandardItem(QString(productBuf));
			item->setData(QVariant::fromValue(USBDevice(dev)), USBDeviceRole);
			parent->appendRow(item);
		}
		catch (LIBUSBError& e) {
			// TODO: warning?
		}
	}
	return model;
}

void MainWindow::keyboardSelected(int index) {
	USBDevice v = mKeyboardsModel->item(index)->data(USBDeviceRole).value<USBDevice>();
	KeyboardComm comm(v);

	mKeyboardValues->layoutID->setText(QString::number((int) comm.getLayoutID()));
	mKeyboardValues->mappingSize->setText(QString::number((int) comm.getMappingSize()));
	mKeyboardValues->numPrograms->setText(QString::number((int) comm.getNumPrograms()));
	mKeyboardValues->programSpaceRaw->setText(QString::number((int) comm.getProgramSpaceRaw()));
	mKeyboardValues->programSpace->setText(QString::number((int) comm.getProgramSpace()));
	mKeyboardValues->macroIndexSize->setText(QString::number((int) comm.getMacroIndexSize()));
	mKeyboardValues->macroStorageSize->setText(QString::number((int) comm.getMacroStorageSize()));
}
