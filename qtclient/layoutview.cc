#include <Qt>
#include <QPainter>
#include <QMouseEvent>
#include <QDebug>
#include "layoutview.h"
#include "layoutpresenter.h"
#include "layout.h"
#include "mapping.h"
#include "keyselectionview.h"
#include "hidtables.h"

LayoutView::LayoutView(LayoutPresenter *presenter)
	: mPresenter(presenter)
	, mKeySelectionView(NULL)
	, mShowingMainLayer(true)
{
	setAlignment(Qt::AlignLeft | Qt::AlignTop);
	setMargin(0);

	mKeypadColor = QColor::fromRgbF(0, 0, 1, 0.2);
	mSelectedColor = QColor::fromRgbF(1, 0, 0, 0.2);
	mUpdatingKeyIndex = -1;
}

void LayoutView::setKeyboard(const Layout *layout, const QPixmap& pixmap) {
	mLayout = layout;
	setPixmap(pixmap);
}

void LayoutView::setMapping(Mapping *m) {
	mMapping = m;
	update();
}

void LayoutView::paintEvent(QPaintEvent *ev) {
	QLabel::paintEvent(ev);

	const QList<uint8_t>& usages = mShowingMainLayer ?
		mMapping->getMainLayer() : mMapping->getKeypadLayer();

	if (!mLayout || usages.empty()) return;

	QPainter painter(this);
	QTextOption buttonTextOption;
	buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);
	buttonTextOption.setWrapMode(QTextOption::WrapAtWordBoundaryOrAnywhere);

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		int offset = it - mLayout->keys.constBegin();
		if (!mShowingMainLayer) {
			painter.fillRect(it->rect, mKeypadColor);
		}
		if (offset == mUpdatingKeyIndex) {
			painter.fillRect(it->rect, mSelectedColor);
		}
		painter.drawText(it->rect,
		                 HIDTables::nameUsage(usages[offset]),
		                 buttonTextOption);
	}
}


void LayoutView::mousePressEvent(QMouseEvent *ev) {
	QLabel::mousePressEvent(ev);
	if (!mLayout) return;

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		if (it->rect.contains(ev->pos())) {
			if (it->name == "LOGICAL_KEY_KEYPAD") {
				mShowingMainLayer = !mShowingMainLayer;
				update();
			}
			else {
				if (!mKeySelectionView) {
					mKeySelectionView = new KeySelectionView(this);
					connect(mKeySelectionView,
							SIGNAL(usageSelected(QString, uint8_t)),
							this,
							SLOT(usageSelected(QString, uint8_t)));
					// TODO track mKeySelection hide
				}
				mKeySelectionView->show();
				mUpdatingKeyIndex = it - mLayout->keys.constBegin();
				update();
			}
		}
	}
}

void LayoutView::usageSelected(QString name, uint8_t usage) {
	if (mUpdatingKeyIndex < 0) {
		qDebug() << "got key without selection?!";
		return;
	}
	mPresenter->setUsage(mShowingMainLayer, mUpdatingKeyIndex, usage);
	mUpdatingKeyIndex = -1;
	update();
}
