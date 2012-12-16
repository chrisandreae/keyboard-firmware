#include <QAbstractItemModel>
#include <QVBoxLayout>
#include <QTableView>
#include <QLabel>
#include <QTextEdit>
#include <QItemSelectionModel>

#include "programsview.h"
#include "programspresenter.h"
#include "programsitemmodel.h"

ProgramsView::ProgramsView(ProgramsPresenter *presenter, ProgramsItemModel *programs)
	: mPresenter(presenter)
	, mPrograms(programs)
{
	QGridLayout *layout = new QGridLayout;
	layout->addWidget(mProgramsSize = new QLabel, 0, 0);
	layout->addWidget(mProgramsTable = new QTableView, 1, 0);
	layout->addWidget(mProgramDump = new QLabel, 1, 1, 2, 1);

	mProgramsTable->setShowGrid(false);
	mProgramsTable->setSelectionBehavior(QAbstractItemView::SelectRows);
	mProgramsTable->setSelectionMode(QAbstractItemView::SingleSelection);

	setLayout(layout);
}

void ProgramsView::updateProgramsSize() {
	unsigned int total = 0;
	if (mPrograms) {
		for (int row = 0; row < mPrograms->rowCount(); row++) {
			uint16_t programSize =
			    mPrograms->data(mPrograms->index(row, (int) ProgramsItemModel::SizeColumn),
			                    ProgramsItemModel::RawData)
			    .toInt();
			total += programSize;
		}
		mProgramsSize->setText(QString("Programs Size: %1/%2")
		                       .arg(total)
		                       .arg(mPrograms->getProgramSpace()));
	}
}

void ProgramsView::selectedProgram(const QModelIndex& index) {
	mProgramDump->setText(index.data().toString());
}


void ProgramsView::setModel(ProgramsItemModel *programs) {
	mProgramDump->setText("");

	mPrograms = programs;
	QItemSelectionModel *selModel = new QItemSelectionModel(programs, programs);
	mProgramsTable->setModel(programs);
	mProgramsTable->setSelectionModel(selModel);
	updateProgramsSize();

	connect(mPrograms, SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&)),
	        this, SLOT(updateProgramsSize()));
	connect(selModel, SIGNAL(currentChanged(const QModelIndex&, const QModelIndex&)),
			this, SLOT(selectedProgram(const QModelIndex&)));

}
