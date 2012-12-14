// -*- c++ -*-
#ifndef LAYOUTPRESENTER_H
#define LAYOUTPRESENTER_H

#include <QObject>

#include "layoutview.h"

class KeyboardModel;

class LayoutPresenter : public QObject {
	Q_OBJECT

	LayoutView *mView;
	KeyboardModel *mModel;

public:
	LayoutPresenter();
	~LayoutPresenter();
	QWidget *getWidget() { return mView; }

public slots:
	void setModel(KeyboardModel *model);
};

#endif
