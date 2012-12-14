// -*- c++ -*-
#ifndef LAYOUTVIEW_H
#define LAYOUTVIEW_H

#include <QLabel>
#include <QPixmap>

class LayoutPresenter;

class LayoutView : public QLabel {
	Q_OBJECT
	Q_DISABLE_COPY(LayoutView)

	LayoutPresenter *mPresenter;

public:
	LayoutView(LayoutPresenter *presenter);

	void setKeyboardImage(const QPixmap& pixmap);
	// void setKeyboardInfo(uint8_t layoutID);
};

#endif
