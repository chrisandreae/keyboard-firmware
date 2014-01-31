#include <QtGui>

#include "triggersitemdelegate.h"
#include "trigger.h"


namespace {

enum {
	TriggerTypeRole = Qt::UserRole + 1,
	ProgramNumberRole = Qt::UserRole + 2,
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

QStandardItemModel *TriggersItemDelegate::programsModel(QObject *parent) const {
	QStandardItemModel *pm = new QStandardItemModel(parent);
	int nPrograms = mPresenter->getNumPrograms();
	for (int i = 0; i < nPrograms; ++i) {
		// to avoid a bunch of +1 and -1, we store both the display
		// name the internal program index in the model.
		const int programDisplayNumber = i + 1;
		QStandardItem *item = new QStandardItem(
			QString("Program %1").arg(programDisplayNumber));
		item->setData(i, ProgramNumberRole);
		pm->appendRow(item);
	}
	return pm;
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
	else if (isProgramField(index)) {
		QComboBox *comboBox = new QComboBox(parent);
		comboBox->setModel(programsModel(comboBox));
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
	else if (isProgramField(index)) {
		QComboBox *comboBox = static_cast<QComboBox*>(editor);
		QVariant data = index.data(Qt::EditRole);
		comboBox->setCurrentIndex(comboBox->findData(data, ProgramNumberRole));
	}
	else
		QStyledItemDelegate::setEditorData(editor, index);
}

void TriggersItemDelegate::setModelData(QWidget *editor, QAbstractItemModel *model,
										const QModelIndex& index) const
{
	bool typeField = isTypeField(index);
	bool programField = isProgramField(index);
	if (typeField || programField) {
		int usage;
		if (typeField)
			usage = TriggerTypeRole;
		else if (programField)
			usage = ProgramNumberRole;

		QComboBox *comboBox = static_cast<QComboBox*>(editor);
		QModelIndex selectedIndex =
			comboBox->model()->index(comboBox->currentIndex(), 0);
		QVariant data = selectedIndex.data(usage);
		model->setData(index, data, Qt::EditRole);
	}
	else
		QStyledItemDelegate::setModelData(editor, model, index);
}

void TriggersItemDelegate::updateEditorGeometry(QWidget *editor,
												const QStyleOptionViewItem& option,
												const QModelIndex& index) const
{
	if (isTypeField(index) || isProgramField(index)) {
		QComboBox *comboBox = static_cast<QComboBox*>(editor);
		comboBox->setGeometry(option.rect);
	}
	else
		QStyledItemDelegate::updateEditorGeometry(editor, option, index);
}

