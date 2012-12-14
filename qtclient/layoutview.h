// -*- c++ -*-
#ifndef LAYOUTVIEW_H
#define LAYOUTVIEW_H

#include <QLabel>
#include <QPixmap>

class Mapping;
class Layout;

class LayoutView : public QLabel {
	Q_OBJECT
	Q_DISABLE_COPY(LayoutView)

	QList<uint8_t> mKeyUsages;
	const Layout *mLayout;
	QColor *mBackgroundColor;

public:
	LayoutView();

	void setKeyboard(const Layout *layout, const QPixmap& pixmap);
	void setKeyUsages(const QList<uint8_t>& usages, QColor* backgroundColor);
	virtual void paintEvent(QPaintEvent* e);
	virtual void mousePressEvent(QMouseEvent* e);

signals:
	void buttonClicked(int index, QString name);
};

#endif
