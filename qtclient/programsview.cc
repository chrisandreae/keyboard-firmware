#include <QAbstractItemModel>
#include <QVBoxLayout>
#include <QTableView>
#include <QLabel>
#include <QTextEdit>
#include <QItemSelectionModel>
#include <QFileDialog>
#include <QAction>
#include <QPushButton>
#include <QDebug>
#include <QHeaderView>

#include "programsview.h"
#include "programspresenter.h"
#include "programsitemmodel.h"
#include "util.h"

ProgramsView::ProgramsView(ProgramsPresenter *presenter)
	: mPresenter(presenter)
	, mProgramsSelection(NULL)
	, mProgramSelectDialog(NULL)
{
	QGridLayout *layout = new QGridLayout;
	layout->addWidget(mProgramsSize = new QLabel, 0, 0, 1, 2);
	layout->addWidget(mProgramsTable = new QTableView, 1, 0, 1, 2);
	layout->addWidget(mProgramDump = new QTextEdit, 1, 3, 2, 1);

	mProgramDump->setReadOnly(true);

	mProgramsTable->setShowGrid(false);
	mProgramsTable->setSelectionBehavior(QAbstractItemView::SelectRows);
	mProgramsTable->setSelectionMode(QAbstractItemView::SingleSelection);

	mProgramsTable->horizontalHeader()->setStretchLastSection(true);
	mProgramsTable->horizontalHeader()->setResizeMode(QHeaderView::Stretch);
	mProgramsTable->verticalHeader()->setVisible(false);


	mLoadButton = new QPushButton(tr("Load Program"), this);
	connect(mLoadButton, SIGNAL(clicked()),
	        this, SLOT(loadProgram()));
	layout->addWidget(mLoadButton, 2, 0);

	mClearButton = new QPushButton(tr("Clear Program"), this);
	connect(mClearButton, SIGNAL(clicked()),
	        this, SLOT(clearProgram()));
	layout->addWidget(mClearButton, 2, 1);

	setLayout(layout);
}

ProgramsView::~ProgramsView()
{
}

void ProgramsView::updatePrograms() {
	unsigned int total = 0;
	if (mPrograms) {
		for (QList<Program>::const_iterator it = mPrograms->constBegin();
		     it != mPrograms->constEnd();
		     ++it)
		{
			total += it->length();
		}
		mProgramsSize->setText(QString("Programs Size: %1/%2")
		                       .arg(total)
		                       .arg(mProgramSpace));
	}
	if (mProgramsSelection) {
		selectedProgram(mProgramsSelection->currentIndex());
	}
}

void ProgramsView::selectedProgram(const QModelIndex& index) {
	if (index.isValid()) {
		QString programDump = "<pre>";
		const Program& program = mPrograms->at(index.row());
		if (program.length() != 0) {
			programDump += Program::disassemble(program.getByteCode());
		}
		programDump += "</pre>";
		mProgramDump->setText(programDump);
	}

	mLoadButton->setEnabled(index.isValid());
	mClearButton->setEnabled(index.isValid() &&
	                         mPrograms->at(index.row()).getByteCode().length() != 0);
}


void ProgramsView::setPrograms(const QList<Program> *programs, int programSpace) {
	mPrograms = programs; // FIXME: alias without ownership
	mProgramSpace = programSpace;

	mProgramsModel.reset(
	    new ProgramsItemModel(*programs, NULL));

	mProgramDump->setText("");

	mProgramsSelection =
		new QItemSelectionModel(mProgramsModel.data(), mProgramsModel.data());
	mProgramsTable->setModel(mProgramsModel.data());
	mProgramsTable->setSelectionModel(mProgramsSelection);
	updatePrograms();

	connect(mProgramsModel.data(), SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&)),
	        this, SLOT(updatePrograms()));
	connect(mProgramsSelection, SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)),
	        this, SLOT(handleSelectionChanged()));

}

void ProgramsView::loadProgram() {
	if (!mProgramsSelection->hasSelection()) {
		qDebug() << "requested load with no selection";
		return;
	}

	if (!mProgramSelectDialog) {
		mProgramSelectDialog = new QFileDialog(this);
	}
	mProgramSelectDialog->setNameFilter("*.k *.kc");
	mProgramSelectDialog->open(this, SLOT(fileSelected(const QString&)));
}

void ProgramsView::clearProgram() {
	if (!mProgramsSelection->hasSelection()) {
		qDebug() << "requested clear with no selection";
		return;
	}
	mPresenter->setProgram(mProgramsSelection->currentIndex().row(),
	                       QByteArray());
}

void ProgramsView::fileSelected(const QString& filename) {
	if (filename.endsWith(".kc")) {
		mPresenter->setProgram(mProgramsSelection->currentIndex().row(),
		                       filename);
	}
	else {
		QFile file(filename);
		if (file.open(QFile::ReadOnly)) {
			QByteArray contents = file.readAll();
			mPresenter->setProgram(mProgramsSelection->currentIndex().row(),
			                       contents);
		}
		else {
			qDebug() << "Failed to read program";
		}
	}
}


void ProgramsView::programChanged(int index) {
	mProgramsModel->sendChanged(index);
}

void ProgramsView::handleSelectionChanged() {
	selectedProgram(currentSelectionOf(*mProgramsSelection));
}
