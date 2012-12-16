// -*- c++ -*-
#ifndef PROGRAMSPRESENTER_H
#define PROGRAMSPRESENTER_H

#include <QObject>
#include <QScopedPointer>

#include "programsview.h"

class KeyboardModel;
class ProgramsItemModel;

class ProgramsPresenter : public QObject {
	Q_OBJECT

	ProgramsView *mView;
	QScopedPointer<ProgramsItemModel> mItemModel;

public:
	ProgramsPresenter();
	~ProgramsPresenter();

	QWidget *getWidget() { return mView; }

public slots:
	void setModel(KeyboardModel *model);
};

#endif
