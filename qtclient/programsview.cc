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

#include "programsview.h"
#include "programspresenter.h"
#include "programsitemmodel.h"

ProgramsView::ProgramsView(ProgramsPresenter *presenter)
	: mPresenter(presenter)
	, mProgramsSelection(NULL)
	, mProgramSelectDialog(NULL)
{
	QGridLayout *layout = new QGridLayout;
	layout->addWidget(mProgramsSize = new QLabel, 0, 0);
	layout->addWidget(mProgramsTable = new QTableView, 1, 0);
	layout->addWidget(mProgramDump = new QTextEdit, 1, 1, 3, 1);

	mProgramDump->setReadOnly(true);

	mProgramsTable->setShowGrid(false);
	mProgramsTable->setSelectionBehavior(QAbstractItemView::SelectRows);
	mProgramsTable->setSelectionMode(QAbstractItemView::SingleSelection);

	mLoadButton = new QPushButton(tr("Load Program"), this);
	connect(mLoadButton, SIGNAL(clicked()),
	        this, SLOT(loadProgram()));
	layout->addWidget(mLoadButton, 2, 0);

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
		// TODO: this hurts me
		QString programDump = "<pre>";
		const Program& program = mPrograms->at(index.row());
		const char *start = program.getByteCode();
		const char *end = start + program.length();
		const char *byteCode = start;
		while (byteCode < end) {
			const char *nextInstruction = byteCode;
			QString prettyInstruction = Program::prettyPrintInstruction(&nextInstruction);
			QString bytes;
			for (const char *p = byteCode; p < nextInstruction; ++p) {
				bytes += QString("%1 ").arg((uint8_t) *p, 2, 16, QLatin1Char('0'));
			}
			programDump += QString("%1: %2 %3\n")
				.arg(byteCode - start, 8, 16, QLatin1Char('0'))
				.arg(bytes, -15)
				.arg(prettyInstruction);

			byteCode = nextInstruction;
		}
		programDump += "</pre>";
		mProgramDump->setText(programDump);
	}

	mLoadButton->setEnabled(index.isValid());
}


void ProgramsView::setPrograms(const QList<Program> *programs, int programSpace) {
	mPrograms = programs;
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
	connect(mProgramsSelection, SIGNAL(currentChanged(const QModelIndex&, const QModelIndex&)),
	        this, SLOT(selectedProgram(const QModelIndex&)));

}

void ProgramsView::loadProgram() {
	if (!mProgramsSelection->hasSelection()) {
		qDebug() << "requested load with no selection";
		return;
	}

	if (!mProgramSelectDialog) {
		mProgramSelectDialog = new QFileDialog(this);
	}
	mProgramSelectDialog->setNameFilter("*.k");
	mProgramSelectDialog->open(this, SLOT(fileSelected(const QString&)));
}

void ProgramsView::fileSelected(const QString& filename) {
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


void ProgramsView::programChanged(int index) {
	mProgramsModel->sendChanged(index);
}
