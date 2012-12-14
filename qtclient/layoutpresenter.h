// -*- c++ -*-
#ifndef LAYOUTPRESENTER_H
#define LAYOUTPRESENTER_H

#include <QObject>
#include <QScopedPointer>

#include "layoutview.h"

class KeyboardModel;

class LayoutPresenter : public QObject {
	Q_OBJECT

	LayoutView *mView;
	KeyboardModel *mModel;
	QScopedPointer<Layout> mLayout;
	QScopedPointer<Mapping> mMapping;

	bool mShowingKeypad;
public:
	LayoutPresenter();
	~LayoutPresenter();
	QWidget *getWidget() { return mView; }

public slots:
	void setModel(KeyboardModel *model);
	void handleButton(int index, QString name);
};

#endif
