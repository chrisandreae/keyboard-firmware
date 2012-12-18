#include <QPainter>
#include <QMouseEvent>
#include <QDebug>

#include "layoutwidget.h"
#include "hidtables.h"
#include "layout.h"

void LayoutWidget::paintEvent(QPaintEvent *ev) {
	QLabel::paintEvent(ev);

	if (!mLayout) return;

	QPainter painter(this);

	if (mUsages.empty() && mBackgroundColor == Qt::transparent) {
		qDebug() << "rendering fast path";
		// TODO: this is incompatible with background color
		// drawing without usages, just draw selection
		for (QSet<uint8_t>::const_iterator it = mSelection.constBegin();
		     it != mSelection.constEnd();
		     ++it)
		{
			painter.fillRect(scaleRect(mLayout->keys[*it].rect),
							 mSelectedColor);
		}
	}
	else {
		qDebug() << "rendering slow path";
		QFont defaultFont = painter.font();
		QTextOption buttonTextOption;
		buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);

		for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
			 it != mLayout->keys.constEnd();
			 ++it)
		{
			QRect keyRect = scaleRect(it->rect);
			int offset = it - mLayout->keys.constBegin();
			painter.fillRect(keyRect, mBackgroundColor);
			if (mSelection.contains(offset)) {
				painter.fillRect(keyRect, mSelectedColor);
			}
			if (!mUsages.empty()) {
				QString keyLabel =
					QString(HIDTables::nameUsage(mUsages[offset])).replace('_', '\n');
				QRectF keyBounds(keyRect);
				while (!keyBounds.contains(
						   painter.boundingRect(keyRect, keyLabel, buttonTextOption)))
				{
					QFont f = painter.font();
					f.setPointSize(f.pointSize() - 1);
					painter.setFont(f);
				}

				painter.drawText(keyRect, keyLabel, buttonTextOption);
				painter.setFont(defaultFont);
			}
		}
	}
}

void LayoutWidget::setKeyboardLayout(const Layout *layout) {
	mLayout = layout;
	QPixmap image =
		QPixmap(QString(":layout/%1").arg(layout->imageName));
	if (mScale != 1.0f) {
		setPixmap(image.scaledToWidth(image.width() * mScale,
		                              Qt::SmoothTransformation));
	}
	else {
		setPixmap(image);
	}
}

void LayoutWidget::mousePressEvent(QMouseEvent *ev) {
	QLabel::mousePressEvent(ev);
	if (!mLayout) return;

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		if (scaleRect(it->rect).contains(ev->pos())) {
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
