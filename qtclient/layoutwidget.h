// -*- c++ -*-

#ifndef LAYOUTWDIGET_H
#define LAYOUTWDIGET_H

#include <stdint.h>
#include <QLabel>
#include <QColor>
#include <QSet>
#include <QList>
#include <QRect>

class Layout;
class QPaintEvent;
class QMouseEvent;
class QPixmap;

class LayoutWidget : public QLabel {
	Q_OBJECT

	QColor mBackgroundColor;
	QColor mSelectedColor;

	const Layout *mLayout;
	QSet<uint8_t> mSelection;
	QList<uint8_t> mUsages;
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
		, mBackgroundColor(Qt::transparent)
		, mScale(1.0f)
	{
		setAlignment(Qt::AlignLeft | Qt::AlignTop);
		mSelectedColor = QColor::fromRgbF(1, 0, 0, 0.2);
	}

	void setScale(float f) { mScale = f; }
	void setKeyboardLayout(const Layout *layout);
	void setUsages(const QList<uint8_t>& usages);

	void setSelection(const QSet<uint8_t>& selectedKeys);
	void setSelection(uint8_t selectedKey);

	void setBackgroundColor(const QColor& color);

	void paintEvent(QPaintEvent *ev);
	void mousePressEvent(QMouseEvent *ev);

signals:
	void buttonClicked(int index);

};

#endif
