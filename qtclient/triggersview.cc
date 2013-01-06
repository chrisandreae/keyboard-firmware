#include <QGridLayout>
#include <QLabel>
#include <QTableView>
#include <QHeaderView>
#include <QModelIndex>
#include <QItemSelectionModel>
#include <QPushButton>
#include <QDebug>

#include "trigger.h"
#include "triggersview.h"
#include "triggerspresenter.h"
#include "triggersitemmodel.h"
#include "triggersitemdelegate.h"
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
	mTableView->setItemDelegate(new TriggersItemDelegate(mTableView, presenter));
	mTableView->setEditTriggers(QAbstractItemView::AllEditTriggers);

	layout->addWidget(mTableView, 0, 0, 1, 2);

	mAddTriggerButton = new QPushButton(tr("Add Trigger"));
	layout->addWidget(mAddTriggerButton, 1, 0);
	connect(mAddTriggerButton, SIGNAL(clicked()), this, SLOT(appendTrigger()));

	mRemoveTriggerButton = new QPushButton(tr("Remove Trigger"));
	layout->addWidget(mRemoveTriggerButton, 1, 1);
	connect(mRemoveTriggerButton, SIGNAL(clicked()), this, SLOT(removeTrigger()));

	mTriggerSetWidget = new LayoutWidget;
	mTriggerSetWidget->setScale(0.4f);
	layout->addWidget(mTriggerSetWidget, 2, 0);

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

void TriggersView::setKeyboardLayout(const Layout& keyboardLayout) {
	mKeyboardLayout = &keyboardLayout; // FIXME: aliasing without ownership
	mTriggerSetWidget->setKeyboardLayout(keyboardLayout);
}

TriggersView::~TriggersView()
{
}

void TriggersView::appendTrigger() {
	mPresenter->appendTrigger();
}

void TriggersView::removeTrigger() {
	if (mSelection->hasSelection()) {
		mPresenter->removeTrigger(mSelection->currentIndex().row());
	}
	else{
		qDebug() << "remove trigger without selection";
	}
}

void TriggersView::beforeTriggersChanged() {
	mItemModel->beforeTriggersChanged();
}
void TriggersView::afterTriggersChanged() {
	mItemModel->afterTriggersChanged();
}

void TriggersView::triggerChanged(int index) {
	mItemModel->triggerChanged(index);
}

void TriggersView::beforeInsertTrigger(int index) {
	mItemModel->beforeInsertTrigger(index);
}

void TriggersView::afterInsertTrigger() {
	mItemModel->afterInsertTrigger();
}

void TriggersView::beforeRemoveTrigger(int index) {
	mItemModel->beforeRemoveTrigger(index);
}

void TriggersView::afterRemoveTrigger() {
	mItemModel->afterRemoveTrigger();
}


/**
 * Update the graphical display of the trigger for the currently selected row
 */
void TriggersView::updateTriggerSetWidget(const QModelIndex& index){
	if(index.isValid()){
		const Trigger *currentTrigger = mPresenter->getTrigger(index.row());
		mTriggerSetWidget->setSelection(QSet<LogicalKeycode>::fromList(currentTrigger->triggerKeys()));
	}
	else{
		mTriggerSetWidget->setSelection(QSet<LogicalKeycode>());
	}
}

static const QModelIndex currentSelectionOf(const QItemSelectionModel& selectionModel){
	return selectionModel.currentIndex();
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
	updateButtons();
}


void TriggersView::updateButtons() {
	mRemoveTriggerButton->setEnabled(mSelection->hasSelection());
}

void TriggersView::handleSelectionChange(const QItemSelection& current,
                                         const QItemSelection& previous)
{
	Q_UNUSED(current);
	Q_UNUSED(previous);

	updateTriggerSetWidget(currentSelectionOf(*mSelection));
	updateButtons();
}

void TriggersView::handleLogicalKeyClicked(LogicalKeycode logicalKeycode) {
	if(mSelection->hasSelection()) {
		int triggerIndex = currentSelectionOf(*mSelection).row();
		mPresenter->toggleKeyInTrigger(triggerIndex, logicalKeycode);
	}
}
