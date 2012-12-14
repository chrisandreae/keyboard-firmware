// -*- c++ -*-
#ifndef LAYOUTVIEW_H
#define LAYOUTVIEW_H

#include <QWidget>

class LayoutPresenter;

class LayoutView : public QWidget {
	Q_OBJECT
	Q_DISABLE_COPY(LayoutView)

	LayoutPresenter *mPresenter;

public:
	LayoutView(LayoutPresenter *presenter);

	void setKeyboardInfo(uint8_t layoutID);
};

#endif
