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
	return createIndex(row, column, (void*) &mPresenter->getTrigger(row));
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
	return 2 + mPresenter->getKeysPerTrigger();
}

QVariant TriggersItemModel::data(const QModelIndex& index, int role) const {
	const Trigger* t = static_cast<const Trigger *>(index.internalPointer());
	int col = index.column();
	if(col < mPresenter->getKeysPerTrigger()) {
		const QList<LogicalKeycode>& triggerKeys = t->triggerKeys();
		if(col >= triggerKeys.count()){
			// Trigger has fewer than max columns, light grey placeholder
			if (role == Qt::DisplayRole)
				return QString("-");
			else if (role == Qt::ForegroundRole)
				return QBrush(QColor::fromRgbF(0.6, 0.6, 0.6));
		}
		else if (role == Qt::DisplayRole) {
			// Format the name of this trigger key
			QList<LogicalKeycode> keyList = t->triggerKeys();
			std::reverse(keyList.begin(), keyList.end());
			// qSort(keyList.begin(), keyList.end(), qGreater<LogicalKeycode>());
			// qReverse(keyList);
			return QString(mPresenter->getLayout().namePosition(keyList.at(col)));
		}
	}
	// Format the type and contents fields
	else if(role == Qt::DisplayRole){
		switch (index.column() - mPresenter->getKeysPerTrigger()) {
		case 0:
			return Trigger::nameType(t->type());
		case 1:
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
		if(section < mPresenter->getKeysPerTrigger()) {
			return QString("Key %1").arg(section + 1);
		}

		switch (section - mPresenter->getKeysPerTrigger()) {
		case 0: return QString("Type");
		case 1: return QString("Contents");
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
