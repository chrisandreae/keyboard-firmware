// -*- c++ -*-
#ifndef TRIGGERSITEMMODEL_H
#define TRIGGERSITEMMODEL_H

#include <QObject>
#include <QAbstractItemModel>

#include "triggerspresenter.h"

class TriggersItemModel : public QAbstractItemModel {
	Q_OBJECT

	TriggersPresenter *mPresenter;

public:
	TriggersItemModel(TriggersPresenter *mPresenter);

	QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
	QModelIndex parent(const QModelIndex& index) const;
	int rowCount(const QModelIndex& parent = QModelIndex()) const;
	int columnCount(const QModelIndex& parent = QModelIndex()) const;
	QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

	QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;

	bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
	Qt::ItemFlags flags(const QModelIndex& index) const;


	inline int typeFieldIndex() const {
		return mPresenter->getKeysPerTrigger();
	}
	inline int contentsFieldIndex() const {
		return mPresenter->getKeysPerTrigger() + 1;
	}

	inline bool isTypeField(const QModelIndex& index) const {
		return index.column() == typeFieldIndex();
	}

	bool isProgramField(const QModelIndex& index) const;

public slots:
	void triggerChanged(int index);
	void beforeTriggersChanged();
	void afterTriggersChanged();
};

#endif
