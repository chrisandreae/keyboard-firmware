#include <QDebug>
#include "programsitemmodel.h"

/*
 * Columns are {name, size} (and maybe one day bytecode), there's no
 * tree
 */

QModelIndex ProgramsItemModel::index(int row, int column, const QModelIndex& parent) const {
	if (column < 0 || column > 2 || row < 0 || row > mPrograms.count())
		return QModelIndex();

	return createIndex(row, column, &mPrograms[row]);
}

QModelIndex ProgramsItemModel::parent(const QModelIndex& index) const {
	return QModelIndex();
}
int ProgramsItemModel::rowCount(const QModelIndex& parent) const {
	return mPrograms.count();
}
int ProgramsItemModel::columnCount(const QModelIndex& parent) const {
	return 2;
}
QVariant ProgramsItemModel::data(const QModelIndex& index, int role) const {
	Program *p = static_cast<Program *>(index.internalPointer());
	switch (index.column()) {
	case NameColumn:
		if (role == Qt::DisplayRole)
			return QString("Program %1").arg(index.row() + 1);
		break;
	case SizeColumn:
		if (role == Qt::DisplayRole)
			return QString("%1 bytes").arg(p->length());
		else if (role == ProgramsItemModel::RawData)
			return p->length();
		break;
	}
	return QVariant();
}
QVariant ProgramsItemModel::headerData(int section, Qt::Orientation orientation, int role) const {
	if (role == Qt::DisplayRole && orientation == Qt::Horizontal) {
		switch (section) {
		case NameColumn:
			return QString("Slot");
		case SizeColumn:
			return QString("Size");
		}
	}
	return QAbstractItemModel::headerData(section, orientation, role);
}
