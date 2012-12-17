#include "trigger.h"
#include "triggersitemmodel.h"
#include "triggerspresenter.h"

TriggersItemModel::TriggersItemModel(TriggersPresenter *presenter)
	: mPresenter(presenter)
{
}

QModelIndex TriggersItemModel::index(int row, int column, const QModelIndex& parent) const {
	Q_UNUSED(parent);
	if (row < 0 || row >= rowCount() || column < 0 || column >= columnCount())
		return QModelIndex();
	return createIndex(row, column, (void*) mPresenter->getTrigger(row));
}

QModelIndex TriggersItemModel::parent(const QModelIndex& index) const {
	Q_UNUSED(index);
	return QModelIndex();
}

int TriggersItemModel::rowCount(const QModelIndex& parent) const {
	Q_UNUSED(parent);
	return mPresenter->getNumTriggers();
}

int TriggersItemModel::columnCount(const QModelIndex& parent) const {
	Q_UNUSED(parent);
	return 3;
}

QVariant TriggersItemModel::data(const QModelIndex& index, int role) const {
	if (role == Qt::DisplayRole) {
		const Trigger* t = static_cast<const Trigger *>(
			index.internalPointer());
		switch (index.column()) {
		case 0:
			return Trigger::formatTriggerSet(
				mPresenter->getLayout(), t->triggerSet());
		case 1:
			return Trigger::nameType(t->type());
		case 2:
			if (t->type() == Trigger::Macro) {
				return Trigger::formatMacro(t->macro());
			}
			else if (t->type() == Trigger::Program) {
				return QString("Program %1").arg(t->program());
			}
		}
	}
	return QVariant();
}

QVariant TriggersItemModel::headerData(int section, Qt::Orientation orientation, int role) const {
	if (role == Qt::DisplayRole && orientation == Qt::Horizontal) {
		switch (section) {
		case 0: return QString("Trigger");
		case 1: return QString("Type");
		case 2: return QString("Contents");
		default: return QString();
		}
	}
	else
		return QAbstractItemModel::headerData(section, orientation, role);
}


void TriggersItemModel::triggerChanged(int row) {
	emit dataChanged(index(row, 0), index(row, qMax(columnCount()-1, 0)));
}

void TriggersItemModel::triggersChanged() {
	reset();
}
