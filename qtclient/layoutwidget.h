// -*- c++ -*-

#ifndef LAYOUTWDIGET_H
#define LAYOUTWDIGET_H

#include <stdint.h>
#include <QLabel>
#include <QColor>
#include <QSet>
#include <QList>
#include <QRect>

#include "layout.h"

class QPaintEvent;
class QMouseEvent;
class QPixmap;

class LayoutWidget : public QLabel {
	Q_OBJECT

	bool mKeypadLayerSelected;
	QColor mKeypadLayerColor;
	QColor mSelectedColor;

	const Layout *mLayout;
	QSet<uint8_t> mSelection;
	QByteArray mMapping;
	float mScale;

	inline QRect scaleRect(QRect r) {
		const float s = mScale;
		if (s == 1.0f) return r;
		return QRect(r.x() * s, r.y() * s,
					 r.width() * s, r.height() * s);
	}

public:
	LayoutWidget(QWidget *parent = NULL)
		: QLabel(parent)
		, mKeypadLayerSelected(false)
		, mScale(1.0f)
	{
		setAlignment(Qt::AlignLeft | Qt::AlignTop);
		mSelectedColor 	  = QColor::fromRgbF(1, 0, 0, 0.2);
		mKeypadLayerColor = QColor::fromRgbF(0, 0, 1, 0.2);
	}

	void setScale(float f) { mScale = f; }
	void setKeyboardLayout(const Layout *layout);
	void setMapping(const QByteArray& mapping);

	void setSelection(const QSet<LogicalKeycode>& selectedKeys);
	void setSelection(LogicalKeycode selectedKey);

	void paintEvent(QPaintEvent *ev);
	void mousePressEvent(QMouseEvent *ev);

signals:
	void logicalKeyClicked (LogicalKeycode logicalKeycode);
	void physicalKeyClicked(PhysicalKeycode physicalKeycode);

};

#endif
