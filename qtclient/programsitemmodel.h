// -*- c++ -*-
#ifndef PROGRAMSITEMMODEL_H
#define PROGRAMSITEMMODEL_H

#include <QAbstractItemModel>
#include "program.h"

class ProgramsItemModel : public QAbstractItemModel {
	Q_DISABLE_COPY(ProgramsItemModel)

	QList<Program>& mPrograms;
	const int mProgramSpace;

public:
	ProgramsItemModel(QList<Program>& programs, int programSpace, QObject *parent = NULL)
		: QAbstractItemModel(parent)
		, mPrograms(programs)
		, mProgramSpace(programSpace)
	{}

	QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
	QModelIndex parent(const QModelIndex& index) const;
	int rowCount(const QModelIndex& parent = QModelIndex()) const;
	int columnCount(const QModelIndex& parent = QModelIndex()) const;
	QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

	QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;

	int getProgramSpace() { return mProgramSpace; }

	enum Roles {
		RawData = Qt::UserRole + 1
	};

	enum Columns {
		NameColumn, SizeColumn,
	};
};

#endif
