// -*- c++ -*-
#ifndef TRIGGERSVIEW_H
#define TRIGGERSVIEW_H

#include <QWidget>

#include "trigger.h"
#include "layout.h"

class QTableView;
class TriggersItemModel;
class TriggersPresenter;
class LayoutWidget;
class QItemSelectionModel;
class QModelIndex;

class TriggersView : public QWidget {
	Q_OBJECT

	QScopedPointer<TriggersItemModel> mItemModel;
	QTableView *mTableView;
	LayoutWidget *mTriggerSetView;
	const Layout *mKeyboardLayout;
	QItemSelectionModel *mSelection;
	TriggersPresenter *mPresenter;

	Trigger mEditingTrigger;

public:
	TriggersView(TriggersPresenter *presenter, QWidget *parent = NULL);
	~TriggersView();

	void triggerChanged(int index);
	void triggersChanged();
	void setKeyboardLayout(const Layout *keyboardLayout);

public slots:
	void handleSelectionChange(const QModelIndex& current,
							   const QModelIndex& previous);
	void handleModelChange(const QModelIndex& topLeft,
						   const QModelIndex& bottomRight);
	void handleLogicalKeyClicked(LogicalKeycode position);
};

#endif
