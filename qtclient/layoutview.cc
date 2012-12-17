#include <Qt>
#include <QCursor>
#include <QDebug>
#include <QLayout>
#include <QPushButton>
#include "layoutview.h"
#include "layoutpresenter.h"
#include "layout.h"
#include "layoutwidget.h"
#include "mapping.h"
#include "keyselectionview.h"
#include "hidtables.h"

LayoutView::LayoutView(LayoutPresenter *presenter)
	: mPresenter(presenter)
	, mLayoutWidget(new LayoutWidget)
	, mKeySelectionView(NULL)
	, mShowingMainLayer(true)
{
	QPushButton *loadDefaults;

	QVBoxLayout *layout = new QVBoxLayout;
	layout->addWidget(loadDefaults = new QPushButton(tr("Load Defaults")));
	layout->addWidget(mLayoutWidget);
	setLayout(layout);

	mKeypadColor = QColor::fromRgbF(0, 0, 1, 0.2);
	mUpdatingKeyIndex = -1;

	connect(mLayoutWidget, SIGNAL(buttonClicked(int)),
	        this, SLOT(handleKey(int)));

	connect(loadDefaults, SIGNAL(clicked()),
	        mPresenter, SLOT(loadDefaults()));
}

void LayoutView::setKeyboardLayout(const Layout *layout) {
	mLayout = layout;
	mLayoutWidget->setKeyboardLayout(layout);
}

void LayoutView::setMapping(Mapping *m) {
	mMapping = m;
	mLayoutWidget->setUsages(mShowingMainLayer
	                         ? mMapping->getMainLayer()
	                         : mMapping->getKeypadLayer());
}

void LayoutView::handleKey(int keyIndex) {
	const Layout::Key& key = mLayout->keys[keyIndex];
	if (key.name == "LOGICAL_KEY_KEYPAD") {
		mShowingMainLayer = !mShowingMainLayer;
		mLayoutWidget->setBackgroundColor(
		    mShowingMainLayer ? Qt::transparent : mKeypadColor);
		mLayoutWidget->setUsages(mShowingMainLayer
		                         ? mMapping->getMainLayer()
		                         : mMapping->getKeypadLayer());
	}
	else {
		if (!mKeySelectionView) {
			mKeySelectionView = new KeySelectionView(this);
			connect(mKeySelectionView,
					SIGNAL(usageSelected(QString, uint8_t)),
					this,
					SLOT(usageSelected(QString, uint8_t)));
			connect(mKeySelectionView, SIGNAL(dismissed()),
					this, SLOT(keySelectionFinished()));
		}
		mKeySelectionView->move(QCursor::pos());
		mKeySelectionView->show();
		mUpdatingKeyIndex = keyIndex;
		mLayoutWidget->setSelection(mUpdatingKeyIndex);
	}
}


void LayoutView::usageSelected(QString name, uint8_t usage) {
	Q_UNUSED(name);
	if (mUpdatingKeyIndex < 0) {
		qDebug() << "got key without selection?!";
		return;
	}
	mPresenter->setUsage(mShowingMainLayer, mUpdatingKeyIndex, usage);
}

void LayoutView::keySelectionFinished() {
	mLayoutWidget->setSelection(QSet<uint8_t>());
}
