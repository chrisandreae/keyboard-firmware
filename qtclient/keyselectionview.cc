#include <QGridLayout>
#include <QLineEdit>
#include <QLabel>
#include <QListWidget>
#include <QPushButton>
#include <QStandardItemModel>
#include <QSortFilterProxyModel>
#include <QKeyEvent>
#include <QDebug>

#include "keyselectionview.h"
#include "hidtables.h"
#include "hidusageproxymodel.h"

KeySelectionView::KeySelectionView(QWidget *parent)
	: QWidget(parent)
{
	mHidUsageModel = HIDTables::newUsageModel(this);

	mHidUsageModelProxy = new HIDUsageProxyModel(this);
	mHidUsageModelProxy->setSourceModel(mHidUsageModel);
	mHidUsageModelProxy->setFilterCaseSensitivity(Qt::CaseInsensitive);

	QGridLayout *layout = new QGridLayout;

	mFilter = new QLineEdit;
	mFilter->installEventFilter(this);

	mUsageList = new QListView;
	mUsageList->setModel(mHidUsageModelProxy);
	mUsageList->setEditTriggers(QAbstractItemView::NoEditTriggers);

	connect(mFilter, SIGNAL(textChanged(QString)),
			this, SLOT(filterChanged(QString)));

	connect(mUsageList, SIGNAL(clicked(const QModelIndex&)),
			this, SLOT(sendUsageSelected(const QModelIndex&)));

	layout->addWidget(mFilter, 0, 0);
	layout->addWidget(mUsageList, 1, 0);
	setLayout(layout);

	setWindowFlags(Qt::Popup);
}

void KeySelectionView::show() {
	mFilter->setText("");
	mFilter->setFocus();
	QWidget::show();
}

void KeySelectionView::filterChanged(QString filter) {
	mHidUsageModelProxy->setFilterString(filter);
	mUsageList->setCurrentIndex(mHidUsageModelProxy->index(0, 0));
}

bool KeySelectionView::eventFilter(QObject *obj, QEvent *event) {
	if (obj == mFilter && event->type() == QEvent::KeyPress) {
		QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
		int key = keyEvent->key();
		if (key == Qt::Key_Down) {
			QModelIndex idx = mUsageList->currentIndex();
			QModelIndex newIdx = idx.sibling(idx.row() + 1, 0);
			if (newIdx.isValid())
				mUsageList->setCurrentIndex(newIdx);
		}
		else if (key == Qt::Key_Up) {
			QModelIndex idx = mUsageList->currentIndex();
			QModelIndex newIdx = idx.sibling(idx.row() - 1, 0);
			if (newIdx.isValid())
				mUsageList->setCurrentIndex(newIdx);
		}
		else if (key == Qt::Key_Enter || key == Qt::Key_Return)
		{
			QModelIndex idx = mUsageList->currentIndex();
			if (idx.isValid())
				sendUsageSelected(idx);
		}
		else {
			return false;
		}
		return true;
	}
	return QObject::eventFilter(obj, event);
}

void KeySelectionView::sendUsageSelected(const QModelIndex& idx) {
	QString name = mHidUsageModelProxy->data(idx).toString();
	HIDKeycode hidUsage =
		mHidUsageModelProxy->data(idx, HIDTables::UsageCode).toInt();
	emit hidUsageSelected(name, hidUsage);
	hide();
	emit dismissed();
}


void KeySelectionView::hideEvent(QHideEvent *e) {
	Q_UNUSED(e);
	emit dismissed();
}
