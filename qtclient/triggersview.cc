#include <QGridLayout>
#include <QLabel>
#include <QTableView>
#include <QHeaderView>
#include <QModelIndex>
#include <QItemSelectionModel>

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
	mTableView->setShowGrid(false);
	mTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	mTableView->setSelectionMode(QAbstractItemView::SingleSelection);

	layout->addWidget(mTableView, 0, 0, 1, 2);

	mTriggerSetView = new LayoutWidget;
	mTriggerSetView->setScale(0.4f);
	layout->addWidget(mTriggerSetView, 1, 0);

	mSelection = new QItemSelectionModel(mItemModel.data(), mItemModel.data());
	mTableView->setSelectionModel(mSelection);

	connect(mItemModel.data(), SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&)),
	        this, SLOT(handleModelChange(const QModelIndex&, const QModelIndex&)));
	connect(mSelection, SIGNAL(currentChanged(const QModelIndex&, const QModelIndex&)),
	        this, SLOT(handleSelectionChange(const QModelIndex&, const QModelIndex&)));
	connect(mTriggerSetView, SIGNAL(buttonClicked(int)),
			this, SLOT(handleButtonClicked(int)));

	setLayout(layout);
}

void TriggersView::setKeyboardLayout(const Layout *keyboardLayout) {
	mKeyboardLayout = keyboardLayout;
	mTriggerSetView->setKeyboardLayout(keyboardLayout);
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

void TriggersView::handleModelChange(const QModelIndex& topLeft,
                                     const QModelIndex& bottomRight)
{
	
}

void TriggersView::handleSelectionChange(const QModelIndex& current,
                                         const QModelIndex& previous)
{
	Q_UNUSED(previous);

	if (current.isValid()) {
		mEditingTrigger = *mPresenter->getTrigger(current.row());
		mTriggerSetView->setSelection(mEditingTrigger.triggerSet());
	}
	else {
		mTriggerSetView->setSelection(QSet<uint8_t>());
	}
}

void TriggersView::handleButtonClicked(int position) {
	QSet<uint8_t> s = mEditingTrigger.triggerSet();
	if (!s.contains(position)) {
		s += position;
	}
	else{
		s -= position;
	}
	mEditingTrigger.setTriggerSet(s);
	mTriggerSetView->setSelection(s);
}
