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
	
	QFont defaultFont = painter.font();
	QTextOption buttonTextOption;
	buttonTextOption.setAlignment(Qt::AlignCenter | Qt::AlignVCenter);

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
		 it != mLayout->keys.constEnd();
		 ++it)
	{
		QRect keyRect = scaleRect(it->rect);

		if(mKeypadLayerSelected){
			painter.fillRect(keyRect, mKeypadLayerColor);
		}

		unsigned int physicalKeycode = it - mLayout->keys.constBegin();
		unsigned int logicalKeycode =
			mLayout->physicalKeycodeToLogical(physicalKeycode, mKeypadLayerSelected);

		if (mSelection.contains(logicalKeycode)) {
			painter.fillRect(keyRect, mSelectedColor);
		}

		if (mMapping.length() != 0) {
			QString keyLabel =
				QString(HIDTables::nameUsage(mMapping[logicalKeycode])).replace('_', '\n');
			QRectF keyBounds(keyRect);
			while (!keyBounds.contains(painter.boundingRect(keyRect,
															keyLabel,
															buttonTextOption)))
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

	for (QList<Layout::Key>::const_iterator it = mLayout->keys.constBegin();
	     it != mLayout->keys.constEnd();
	     ++it)
	{
		if (scaleRect(it->rect).contains(ev->pos())) {
			unsigned int physicalIndex = it - mLayout->keys.constBegin();
			if(physicalIndex == mLayout->keypad.keyIndex){
				// keypad button pressed, switch to keypad mode
				mKeypadLayerSelected = !mKeypadLayerSelected;
				update();
			}
			else{
				unsigned int logicalIndex = 
					mLayout->physicalKeycodeToLogical(physicalIndex, mKeypadLayerSelected);
				emit logicalKeyClicked(logicalIndex);
				emit physicalKeyClicked(physicalIndex);
			}
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
