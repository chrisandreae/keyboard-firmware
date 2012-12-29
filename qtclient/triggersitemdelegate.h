// -*- c++ -*-
#ifndef TRIGGERSITEMDELEGATE_H
#define TRIGGERSITEMDELEGATE_H

#include <QStyledItemDelegate>
#include "triggerspresenter.h"
#include "triggersitemmodel.h"

class QWidget;
class QModelIndex;
class QAbstractItemModel;
class QStandardItemModel;

class TriggersItemDelegate : public QStyledItemDelegate {
	Q_OBJECT

	TriggersPresenter *mPresenter;

	mutable QStandardItemModel *mTriggerTypesModel;

	// lazy constructor
	QStandardItemModel *triggerTypesModel() const;

	QStandardItemModel *programsModel(QObject *parent) const;

	inline bool isTypeField(const QModelIndex& index) const {
		return static_cast<const TriggersItemModel*>(index.model())
			->isTypeField(index);
	}
	inline bool isProgramField(const QModelIndex& index) const {
		return static_cast<const TriggersItemModel*>(index.model())
			->isProgramField(index);
	}

public:
	TriggersItemDelegate(QObject *parent, TriggersPresenter *presenter)
		: QStyledItemDelegate(parent)
		, mPresenter(presenter)
		, mTriggerTypesModel(NULL)
	{}

	virtual QWidget *createEditor(QWidget *parent,
	                              const QStyleOptionViewItem& option,
	                              const QModelIndex& index) const;

	virtual void setEditorData(QWidget *editor, const QModelIndex& index) const;

	virtual void setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex& index) const;

	virtual void updateEditorGeometry(QWidget *editor,
	                                  const QStyleOptionViewItem& option,
	                                  const QModelIndex& index) const;
};

#endif
