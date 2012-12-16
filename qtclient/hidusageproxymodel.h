// -*- c++ -*-
#ifndef HIDUSAGEPROXYMODEL_H
#define HIDUSAGEPROXYMODEL_H

#include <QSortFilterProxyModel>
#include <QModelIndex>

class HIDUsageProxyModel : public QSortFilterProxyModel {
	Q_OBJECT

	QString mFilter;

protected:
	virtual bool lessThan(const QModelIndex &left,
	                      const QModelIndex &right)
	    const;

public:
	HIDUsageProxyModel(QObject *parent = NULL)
		: QSortFilterProxyModel(parent)
	{}
	void setFilterString(const QString& filter) {
		mFilter = filter.toCaseFolded();
		QSortFilterProxyModel::setFilterFixedString(filter);
		sort(0);
	}

};

#endif
