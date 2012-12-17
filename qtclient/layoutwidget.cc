#include <QPainter>
#include <QMouseEvent>

#include "layoutwidget.h"
#include "hidtables.h"
#include "layout.h"

void LayoutWidget::paintEvent(QPaintEvent *ev) {
	QLabel::paintEvent(ev);

	if (!mLayout || mUsages.empty()) return;

	QPainter painter(this);
	QFont defaultFont = painter.font();
	QTextOption buttonTextOption;
	buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		int offset = it - mLayout->keys.constBegin();
		painter.fillRect(it->rect, mBackgroundColor);
		if (mSelection.contains(offset)) {
			painter.fillRect(it->rect, mSelectedColor);
		}
		QString keyLabel =
			QString(HIDTables::nameUsage(mUsages[offset])).replace('_', '\n');
		QRectF keyBounds(it->rect);
		while (!keyBounds.contains(
		           painter.boundingRect(it->rect, keyLabel, buttonTextOption)))
		{
			QFont f = painter.font();
			f.setPointSize(f.pointSize() - 1);
			painter.setFont(f);
		}

		painter.drawText(it->rect, keyLabel, buttonTextOption);
		painter.setFont(defaultFont);
	}
}

void LayoutWidget::setKeyboardLayout(const Layout *layout) {
	mLayout = layout;
	setPixmap(QPixmap(QString(":layout/%1").arg(layout->imageName)));
}

void LayoutWidget::mousePressEvent(QMouseEvent *ev) {
	QLabel::mousePressEvent(ev);
	if (!mLayout) return;

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		if (it->rect.contains(ev->pos())) {
			emit buttonClicked(it - mLayout->keys.constBegin());
		}
	}
}

void LayoutWidget::setBackgroundColor(const QColor& color) {
	mBackgroundColor = color;
	update();
}

void LayoutWidget::setSelection(const QSet<uint8_t>& selectedKeys) {
	mSelection = selectedKeys;
	update();
}

void LayoutWidget::setSelection(uint8_t selectedKey) {
	mSelection.clear();
	mSelection << selectedKey;
	update();
}

void LayoutWidget::setUsages(const QList<uint8_t>& usages) {
	mUsages = usages;
	update();
}
