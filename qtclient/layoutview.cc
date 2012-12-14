#include <QPainter>
#include <QMouseEvent>
#include <Qt>
#include "layoutview.h"
#include "layout.h"
#include "hidtables.h"

LayoutView::LayoutView(LayoutPresenter *presenter)
	: mPresenter(presenter)
{
	setAlignment(Qt::AlignLeft | Qt::AlignTop);
	setMargin(0);
}

void LayoutView::setKeyboard(const Layout *layout, const QPixmap& pixmap) {
	mLayout = layout;
	setPixmap(pixmap);
}

void LayoutView::setKeyUsages(const QList<uint8_t>& usages, QColor* backgroundColor) {
	mKeyUsages = usages;
	mBackgroundColor = backgroundColor;
	update();
}

void LayoutView::paintEvent(QPaintEvent *ev) {
	QLabel::paintEvent(ev);
	if (!mLayout || mKeyUsages.empty()) return;

	QPainter painter(this);
	QTextOption buttonTextOption;
	buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);
	buttonTextOption.setWrapMode(QTextOption::WrapAtWordBoundaryOrAnywhere);

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		int offset = it - mLayout->keys.constBegin();
		if (mBackgroundColor) {
			painter.fillRect(it->rect, *mBackgroundColor);
		}
		painter.drawText(it->rect, HIDTables::nameUsage(mKeyUsages[offset]), buttonTextOption);
	}
}


void LayoutView::mousePressEvent(QMouseEvent *ev) {
	QLabel::mousePressEvent(ev);
	if (!mLayout) return;

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		if (it->rect.contains(ev->pos()))
			emit buttonClicked(it - mLayout->keys.constBegin(),
							   it->name);
	}	
}
