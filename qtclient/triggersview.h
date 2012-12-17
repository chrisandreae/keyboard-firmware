// -*- c++ -*-
#ifndef TRIGGERSVIEW_H
#define TRIGGERSVIEW_H

#include <QWidget>

class QTableView;
class TriggersItemModel;
class TriggersPresenter;

class TriggersView : public QWidget {
	Q_OBJECT

	QScopedPointer<TriggersItemModel> mItemModel;
	QTableView *mTableView;

public:
	TriggersView(TriggersPresenter *presenter, QWidget *parent = NULL);
	~TriggersView();

	void triggerChanged(int index);
	void triggersChanged();
};

#endif
