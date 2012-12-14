#include <iostream>

#include <Qt>
#include <QAction>
#include <QBoxLayout>
#include <QComboBox>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QSplitter>
#include <QStackedWidget>
#include <QToolBar>
#include <QToolButton>
#include <QPair>

#include "keyboardview.h"
#include "keyboardcomm.h"
#include "keyboardvalues.h"
#include "keyboardpresenter.h"

enum MyRoles {
	USBDeviceRole = Qt::UserRole + 1,
};

KeyboardView::KeyboardView(QList<QPair<QString, QWidget*> > subviews)
{
	QToolBar *toolBar = addToolBar(tr("Keyboard Selection"));

	mRefreshAction = new QAction(tr("Refresh"), this);

	mKeyboardSelection = new QComboBox;
	toolBar->addWidget(mKeyboardSelection);
	toolBar->addAction(mRefreshAction);

	QStackedWidget *selectionStack =
	    mSelectionStack = new QStackedWidget;

	QLabel *noKeyboardsLabel =
	    new QLabel(tr("No Keyboards Selected"));
	noKeyboardsLabel->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
	selectionStack->addWidget(noKeyboardsLabel);

	QListWidget *subviewList = new QListWidget;
	QStackedWidget *subviewStack = new QStackedWidget;

	subviewList->addItem(tr("Values"));
	subviewStack->addWidget(mKeyboardValues = new KeyboardValues);

	if (!subviews.empty()) {
		for (QList<QPair<QString, QWidget*> >::iterator it = subviews.begin();
			 it != subviews.end();
			 ++it)
		{
			const QPair<QString, QWidget*>& sv = *it;
			subviewList->addItem(sv.first);
			subviewStack->addWidget(sv.second);
		}
	}

	subviewList->addItem(tr("About"));
	subviewStack->addWidget(new QLabel(tr("About")));

	subviewList->setCurrentRow(0);

	connect(subviewList, SIGNAL(currentRowChanged(int)),
	        subviewStack, SLOT(setCurrentIndex(int)));

	QSplitter *splitter = new QSplitter(Qt::Horizontal);
	splitter->addWidget(subviewList);
	splitter->addWidget(subviewStack);
	splitter->setSizes(QList<int>() << 10 << 20);
	selectionStack->addWidget(splitter);

	setCentralWidget(selectionStack);

	setUnifiedTitleAndToolBarOnMac(true);
	setWindowTitle(tr("Keyboard Client"));

	connect(mKeyboardSelection, SIGNAL(currentIndexChanged(int)),
	        this, SIGNAL(selectDeviceAction(int)));

	connect(mRefreshAction, SIGNAL(triggered()),
	        this, SIGNAL(updateDeviceListAction()));
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
	mSelectionStack->setCurrentIndex(0);
}

void KeyboardView::showKeyboard() {
	if (mSelectionStack->currentIndex() != 1)
		mSelectionStack->setCurrentIndex(1);
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
