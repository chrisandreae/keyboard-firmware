// -*- c++ -*-
#ifndef TRIGGERSVIEW_H
#define TRIGGERSVIEW_H

#include <QWidget>

#include "trigger.h"
#include "layout.h"

class QTableView;
class TriggersItemModel;
class TriggersPresenter;
class LayeredLayoutWidget;
class QItemSelectionModel;
class QItemSelection;
class QModelIndex;
class QPushButton;

class TriggersView : public QWidget {
	Q_OBJECT

	QScopedPointer<TriggersItemModel> mItemModel;
	QTableView *mTableView;
	LayeredLayoutWidget *mTriggerSetWidget;
	const Layout *mKeyboardLayout;
	QItemSelectionModel *mSelection;
	TriggersPresenter *mPresenter;
	QPushButton *mAddTriggerButton;
	QPushButton *mRemoveTriggerButton;

	void updateTriggerSetWidget(const QModelIndex& index);
	void updateButtons();

public:
	TriggersView(TriggersPresenter *presenter, QWidget *parent = NULL);
	~TriggersView();

	void triggerChanged(int index);
	void beforeTriggersChanged();
	void afterTriggersChanged();
	void setKeyboardLayout(const Layout& keyboardLayout);

	void beforeInsertTrigger(int index);
	void afterInsertTrigger();
	void beforeRemoveTrigger(int index);
	void afterRemoveTrigger();

public slots:
	void handleSelectionChange(const QItemSelection& current,
							   const QItemSelection& previous);
	void handleModelChange(const QModelIndex& topLeft,
						   const QModelIndex& bottomRight);
	void handleModelReset();

	void handleLogicalKeyClicked(LogicalKeycode position);
	void handleLayerChanged(unsigned currentLayer);

	void appendTrigger();
	void removeTrigger();
};

#endif
