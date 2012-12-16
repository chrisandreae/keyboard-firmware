// -*- c++ -*-
#ifndef PROGRAMSVIEW_h
#define PROGRAMSVIEW_h

#include <QWidget>
#include <QScopedPointer>
#include "program.h"

class ProgramsPresenter;
class ProgramsItemModel;
class QTableView;
class QLabel;
class QTextEdit;
class QModelIndex;

class ProgramsView : public QWidget {
	Q_OBJECT

	ProgramsPresenter *mPresenter;
	ProgramsItemModel *mPrograms;

	QTableView *mProgramsTable;
	QLabel *mProgramsSize;
	QLabel *mProgramDump;

public:
	ProgramsView(ProgramsPresenter *p, ProgramsItemModel *programs = NULL);
	void setModel(ProgramsItemModel *programs);

public slots:
	void updateProgramsSize();
	void selectedProgram(const QModelIndex& index);

};

#endif
