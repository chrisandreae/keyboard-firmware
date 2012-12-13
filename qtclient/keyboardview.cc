#include <iostream>

#include <Qt>
#include <QToolBar>
#include <QBoxLayout>
#include <QLabel>
#include <QToolButton>
#include <QStackedWidget>
#include <QComboBox>
#include <QLineEdit>
#include <QAction>

#include "keyboardview.h"
#include "keyboardcomm.h"
#include "keyboardvalues.h"
#include "keyboardpresenter.h"

enum MyRoles {
	USBDeviceRole = Qt::UserRole + 1,
};

KeyboardView::KeyboardView(KeyboardPresenter *presenter) {
	QToolBar *toolBar = addToolBar(tr("Keyboard Selection"));

	mRefreshAction = new QAction(tr("Refresh"), this);

	mKeyboardSelection = new QComboBox;
	toolBar->addWidget(mKeyboardSelection);
	toolBar->addAction(mRefreshAction);

	QStackedWidget *widgetStack = new QStackedWidget;

	QLabel *noKeyboardsLabel =
		new QLabel(tr("No Keyboards Found\nAttach keyboard and refresh"));
	noKeyboardsLabel->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
	widgetStack->addWidget(noKeyboardsLabel);

	widgetStack->addWidget(mKeyboardValues = new KeyboardValues);
	mWidgetStack = widgetStack;

	setCentralWidget(widgetStack);

	setUnifiedTitleAndToolBarOnMac(true);
	setWindowTitle(tr("Keyboard Client"));

	setPresenter(presenter);
}

void KeyboardView::setPresenter(KeyboardPresenter *presenter) {
	if (mPresenter) {
		disconnect(mKeyboardSelection, NULL,
				   mPresenter, NULL);
		disconnect(mRefreshAction, NULL,
				   mPresenter, NULL);
	}
	connect(mKeyboardSelection, SIGNAL(currentIndexChanged(int)),
			presenter, SLOT(selectDeviceAction(int)));
	connect(mRefreshAction, SIGNAL(triggered()),
			presenter, SLOT(updateDeviceListAction()));
		
}

void KeyboardView::updateDevices(const QStringList& names) {
	mKeyboardSelection->clear();
	for (QStringList::const_iterator it = names.constBegin();
		 it != names.constEnd();
		 ++it)
	{
		mKeyboardSelection->addItem(*it);
	}
}

void KeyboardView::showNoKeyboard() {
	mWidgetStack->setCurrentIndex(0);
}

void KeyboardView::showKeyboard() {
	if (mWidgetStack->currentIndex() != 1)
		mWidgetStack->setCurrentIndex(1);
}

void KeyboardView::showValues(uint8_t layoutID,
							  uint8_t mappingSize,
							  uint8_t numPrograms,
							  uint16_t programSpaceRaw,
							  uint16_t programSpace,
							  uint16_t macroIndexSize,
							  uint16_t macroStorageSize)
{
	mKeyboardValues->layoutID->setText(QString::number(layoutID));
	mKeyboardValues->mappingSize->setText(QString::number(mappingSize));
	mKeyboardValues->numPrograms->setText(QString::number(numPrograms));
	mKeyboardValues->programSpaceRaw->setText(QString::number(programSpaceRaw));
	mKeyboardValues->programSpace->setText(QString::number(programSpace));
	mKeyboardValues->macroIndexSize->setText(QString::number(macroIndexSize));
	mKeyboardValues->macroStorageSize->setText(QString::number(macroStorageSize));
}
