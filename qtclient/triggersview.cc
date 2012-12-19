#include <QGridLayout>
#include <QLabel>
#include <QTableView>
#include <QHeaderView>
#include <QModelIndex>
#include <QItemSelectionModel>
#include <QDebug>

#include "trigger.h"
#include "triggersview.h"
#include "triggerspresenter.h"
#include "triggersitemmodel.h"
#include "layoutwidget.h"

TriggersView::TriggersView(TriggersPresenter *presenter, QWidget *parent) 
	: QWidget(parent)
	, mItemModel(new TriggersItemModel(presenter))
	, mPresenter(presenter)
{
	QGridLayout *layout = new QGridLayout;

	mTableView = new QTableView;
	mTableView->setModel(mItemModel.data());
	mTableView->horizontalHeader()->setStretchLastSection(true);
	mTableView->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
	mTableView->verticalHeader()->setVisible(false);
	mTableView->setShowGrid(false);
	mTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	mTableView->setSelectionMode(QAbstractItemView::SingleSelection);

	layout->addWidget(mTableView, 0, 0, 1, 2);

	mTriggerSetWidget = new LayoutWidget;
	mTriggerSetWidget->setScale(0.4f);
	layout->addWidget(mTriggerSetWidget, 1, 0);

	mSelection = new QItemSelectionModel(mItemModel.data(), mItemModel.data());
	mTableView->setSelectionModel(mSelection);

	connect(mItemModel.data(), SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&)),
	        this, SLOT(handleModelChange(const QModelIndex&, const QModelIndex&)));

	connect(mItemModel.data(), SIGNAL(modelReset()),
	        this, SLOT(handleModelReset()));


	connect(mSelection, SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)),
	        this, SLOT(handleSelectionChange(const QItemSelection&, const QItemSelection&)));

	connect(mTriggerSetWidget, SIGNAL(logicalKeyClicked(LogicalKeycode)),
			this, SLOT(handleLogicalKeyClicked(LogicalKeycode)));

	setLayout(layout);
}

void TriggersView::setKeyboardLayout(const Layout *keyboardLayout) {
	mKeyboardLayout = keyboardLayout;
	mTriggerSetWidget->setKeyboardLayout(keyboardLayout);
}

TriggersView::~TriggersView()
{
}


void TriggersView::triggersChanged() {
	mItemModel->triggersChanged();
}

void TriggersView::triggerChanged(int row) {
	mItemModel->triggerChanged(row);
}

/**
 * Update the graphical display of the trigger for the currently selected row
 */
void TriggersView::updateTriggerSetWidget(const QModelIndex& index){
	if(index.isValid()){
		const Trigger& currentTrigger = mPresenter->getTrigger(index.row());
		mTriggerSetWidget->setSelection(QSet<LogicalKeycode>::fromList(currentTrigger.triggerKeys()));
	}
	else{
		mTriggerSetWidget->setSelection(QSet<LogicalKeycode>());
	}
}

static const QModelIndex currentSelectionOf(const QItemSelectionModel& selectionModel){
	if(selectionModel.hasSelection()){
		return selectionModel.selection().indexes().at(0);
	}
	else{
		return QModelIndex();
	}
}

void TriggersView::handleModelChange(const QModelIndex& topLeft,
                                     const QModelIndex& bottomRight)
{
	Q_UNUSED(topLeft);
	Q_UNUSED(bottomRight);

	updateTriggerSetWidget(currentSelectionOf(*mSelection));
}

void TriggersView::handleModelReset()
{
	updateTriggerSetWidget(currentSelectionOf(*mSelection));
}



void TriggersView::handleSelectionChange(const QItemSelection& current,
                                         const QItemSelection& previous)
{
	Q_UNUSED(current);
	Q_UNUSED(previous);

	updateTriggerSetWidget(currentSelectionOf(*mSelection));
}

void TriggersView::handleLogicalKeyClicked(LogicalKeycode logicalKeycode) {
	if(mSelection->hasSelection()) {
		int triggerIndex = currentSelectionOf(*mSelection).row();
		mPresenter->toggleKeyInTrigger(triggerIndex, logicalKeycode);
	}
}
