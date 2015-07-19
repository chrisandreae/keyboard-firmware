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
	Q_OBJECT;

	QColor mOverlayColor;
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
		, mLayout(NULL)
		, mScale(1.0f)
	{
		setAlignment(Qt::AlignLeft | Qt::AlignTop);
		mSelectedColor = QColor::fromRgbF(1, 0, 0, 0.2);
		mOverlayColor = Qt::transparent;
	}

	void setScale(float f) { mScale = f; }
	void setOverlayColor(const QColor& overlay) {
		mOverlayColor = overlay;
	}
	void setKeyboardLayout(const Layout& layout);
	void setMapping(const QByteArray& mapping);

	void setSelection(const QSet<PhysicalKeycode>& selectedKeys);
	void setSelection(PhysicalKeycode selectedKey);

	void paintEvent(QPaintEvent *ev);
	void mousePressEvent(QMouseEvent *ev);

signals:
	void physicalKeyClicked(PhysicalKeycode physicalKeycode);

private:
	void paintScaledLabel(QPainter& painter, const QString label, const QRect rect);
};

#endif
