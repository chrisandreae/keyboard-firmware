#include <QGridLayout>
#include <QLabel>
#include <QTableView>
#include <QHeaderView>

#include "triggersview.h"
#include "triggersitemmodel.h"

TriggersView::TriggersView(TriggersPresenter *presenter, QWidget *parent) 
	: QWidget(parent)
	, mItemModel(new TriggersItemModel(presenter))
{
	QGridLayout *layout = new QGridLayout;

	QTableView *mTableView = new QTableView;
	mTableView->setModel(mItemModel.data());
	mTableView->horizontalHeader()->setStretchLastSection(true);

	layout->addWidget(mTableView, 0, 0);

	setLayout(layout);
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
