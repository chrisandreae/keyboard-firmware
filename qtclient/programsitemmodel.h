// -*- c++ -*-
#ifndef PROGRAMSITEMMODEL_H
#define PROGRAMSITEMMODEL_H

#include <QAbstractItemModel>
#include "program.h"

class ProgramsItemModel : public QAbstractItemModel {
	Q_DISABLE_COPY(ProgramsItemModel)

	const QList<Program>& mPrograms;

public:
	ProgramsItemModel(const QList<Program>& programs, QObject *parent = NULL)
		: QAbstractItemModel(parent)
		, mPrograms(programs)
	{}

	QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
	QModelIndex parent(const QModelIndex& index) const;
	int rowCount(const QModelIndex& parent = QModelIndex()) const;
	int columnCount(const QModelIndex& parent = QModelIndex()) const;
	QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

	QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;

	void sendChanged(int row);

	enum Columns {
		NameColumn, SizeColumn,
	};
};

#endif
