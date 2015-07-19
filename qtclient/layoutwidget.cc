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
	painter.setRenderHint(QPainter::Antialiasing);

	QFont defaultFont = painter.font();
	QTextOption buttonTextOption;
	buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
		 it != mLayout->keys.constEnd();
		 ++it)
	{
		QRect keyRect = scaleRect(it->rect);

		painter.fillRect(keyRect, mOverlayColor);

		unsigned int physicalKeycode = it - mLayout->keys.constBegin();

		if (mSelection.contains(physicalKeycode)) {
			painter.fillRect(keyRect, mSelectedColor);
		}

		if (mMapping.length() != 0) {
			QString keyLabel =
				QString(HIDTables::nameUsage(mMapping[physicalKeycode])).replace('_', '\n');
			paintScaledLabel(painter, keyLabel, keyRect);
		}
	}
}

void LayoutWidget::paintScaledLabel(QPainter& painter, const QString label, const QRect rect) {
	QFont defaultFont = painter.font();
	QTextOption buttonTextOption;
	buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);

	QRectF bounds(rect);
	while (!bounds.contains(painter.boundingRect(rect, label, buttonTextOption))) {
		QFont f = painter.font();
		f.setPointSize(f.pointSize() - 1);
		painter.setFont(f);
	}

	painter.drawText(rect, label, buttonTextOption);
	painter.setFont(defaultFont);
}

void LayoutWidget::setKeyboardLayout(const Layout& layout) {
	mLayout = &layout; // FIXME: aliasing without ownership
	QPixmap image =
		QPixmap(QString(":layout/%1").arg(layout.imageName));
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

	// FIXME: linear scan hit testing
	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		if (scaleRect(it->rect).contains(ev->pos())) {
			unsigned int physicalIndex = it - mLayout->keys.constBegin();
			emit physicalKeyClicked(physicalIndex);
		}
	}
}

void LayoutWidget::setSelection(const QSet<LogicalKeycode>& selectedKeys) {
	mSelection = selectedKeys;
	update();
}

void LayoutWidget::setSelection(LogicalKeycode selectedKey) {
	mSelection.clear();
	mSelection << selectedKey;
	update();
}

void LayoutWidget::setMapping(const QByteArray& mapping) {
	mMapping = mapping;
	update();
}
