// -*- c++ -*-

#ifndef LAYOUTWDIGET_H
#define LAYOUTWDIGET_H

#include <QLabel>
#include <QColor>
#include <QSet>
#include <QList>

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

public:
	LayoutWidget(QWidget *parent = NULL)
		: QLabel(parent)
		, mBackgroundColor(Qt::transparent)
	{
		setAlignment(Qt::AlignLeft | Qt::AlignTop);
		mSelectedColor = QColor::fromRgbF(1, 0, 0, 0.2);
	}

	void setKeyboard(const Layout *layout, const QPixmap& pixmap);
	void paintEvent(QPaintEvent *ev);
	void mousePressEvent(QMouseEvent *ev);
	void setBackgroundColor(const QColor& color);
	void setSelection(const QSet<uint8_t>& selectedKeys);
	void setSelection(uint8_t selectedKey);
	void setUsages(const QList<uint8_t>& usages);

signals:
	void buttonClicked(int index);

};

#endif
