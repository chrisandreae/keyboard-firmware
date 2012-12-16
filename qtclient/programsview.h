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
class QFileDialog;
class QItemSelectionModel;
class QPushButton;

class ProgramsView : public QWidget {
	Q_OBJECT

	ProgramsPresenter *mPresenter;

	const QList<Program> *mPrograms;
	int mProgramSpace;

	QTableView *mProgramsTable;
	QLabel *mProgramsSize;
	QTextEdit *mProgramDump;
	QScopedPointer<ProgramsItemModel> mProgramsModel;
	QItemSelectionModel *mProgramsSelection;

	QPushButton *mLoadButton;
	QPushButton *mClearButton;
	QFileDialog *mProgramSelectDialog;

public:
	ProgramsView(ProgramsPresenter *p);
	~ProgramsView();
	void setPrograms(const QList<Program> *programs, int programSpace);

public slots:
	void updatePrograms();
	void selectedProgram(const QModelIndex& index);
	void loadProgram();
	void clearProgram();
	void fileSelected(const QString& filename);
	void programChanged(int idx);

};

#endif
