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
class QItemSelection;
class QModelIndex;

class TriggersView : public QWidget {
	Q_OBJECT

	QScopedPointer<TriggersItemModel> mItemModel;
	QTableView *mTableView;
	LayoutWidget *mTriggerSetWidget;
	const Layout *mKeyboardLayout;
	QItemSelectionModel *mSelection;
	TriggersPresenter *mPresenter;

	void updateTriggerSetWidget(const QModelIndex& index);

public:
	TriggersView(TriggersPresenter *presenter, QWidget *parent = NULL);
	~TriggersView();

	void triggerChanged(int index);
	void triggersChanged();
	void setKeyboardLayout(const Layout *keyboardLayout);

public slots:
	void handleSelectionChange(const QItemSelection& current,
							   const QItemSelection& previous);
	void handleModelChange(const QModelIndex& topLeft,
						   const QModelIndex& bottomRight);
	void handleModelReset();
	void handleLogicalKeyClicked(LogicalKeycode position);
};

#endif
