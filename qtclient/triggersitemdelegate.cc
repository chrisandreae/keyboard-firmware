#include <QtGui>

#include "triggersitemdelegate.h"
#include "trigger.h"


namespace {

enum {
	TriggerTypeRole = Qt::UserRole + 1,
};

static const struct { const char *name; Trigger::TriggerType type; } types[] = {
	{ "Macro", Trigger::Macro },
	{ "Program", Trigger::Program },
};

};

QStandardItemModel *TriggersItemDelegate::triggerTypesModel() const {
	if (!mTriggerTypesModel) {
		mTriggerTypesModel = new QStandardItemModel(
			const_cast<TriggersItemDelegate*>(this));

		const int nTypes = sizeof(types) / sizeof(*types);
		for (int t = 0; t < nTypes; ++t) {
			QStandardItem *item = new QStandardItem(types[t].name);
			item->setData(types[t].type, TriggerTypeRole);
			mTriggerTypesModel->appendRow(item);
		}
	}
	return mTriggerTypesModel;
}

QWidget *TriggersItemDelegate::createEditor(QWidget *parent,
											const QStyleOptionViewItem& option,
											const QModelIndex& index) const
{
	if (isTypeField(index)) {
		QComboBox *comboBox = new QComboBox(parent);
		comboBox->setModel(triggerTypesModel());
		comboBox->setInsertPolicy(QComboBox::NoInsert);
		return comboBox;
	}
	else
		return QStyledItemDelegate::createEditor(parent, option, index);
}

void TriggersItemDelegate::setEditorData(QWidget *editor, const QModelIndex& index) const {
	if (isTypeField(index)) {
		QComboBox *comboBox = static_cast<QComboBox*>(editor);
		QVariant data = index.data(Qt::EditRole);
		comboBox->setCurrentIndex(comboBox->findData(data, TriggerTypeRole));
	}
	else
		QStyledItemDelegate::setEditorData(editor, index);
}

void TriggersItemDelegate::setModelData(QWidget *editor, QAbstractItemModel *model,
										const QModelIndex& index) const
{
	if (isTypeField(index)) {
		QComboBox *comboBox = static_cast<QComboBox*>(editor);
		QModelIndex selectedIndex = triggerTypesModel()->index(comboBox->currentIndex(), 0);
		QVariant data = triggerTypesModel()->data(selectedIndex, TriggerTypeRole);
		model->setData(index, data, Qt::EditRole);
	}
	else
		QStyledItemDelegate::setModelData(editor, model, index);
}

void TriggersItemDelegate::updateEditorGeometry(QWidget *editor,
												const QStyleOptionViewItem& option,
												const QModelIndex& index) const
{
	if (isTypeField(index)) {
		QComboBox *comboBox = static_cast<QComboBox*>(editor);
		comboBox->setGeometry(option.rect);
	}
	else
		QStyledItemDelegate::updateEditorGeometry(editor, option, index);
}

