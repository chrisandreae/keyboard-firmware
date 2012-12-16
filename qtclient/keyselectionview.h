// -*- c++ -*-
#ifndef KEYSELECTIONVIEW_H
#define KEYSELECTIONVIEW_H

#include <QWidget>
class QStandardItemModel;
class HIDUsageProxyModel;
class QListView;
class QLineEdit;
class QModelIndex;
class QHideEvent;

class KeySelectionView : public QWidget {
	Q_OBJECT

	QStandardItemModel *mHidUsageModel;
	HIDUsageProxyModel *mHidUsageModelProxy;

	QLineEdit *mFilter;
	QListView *mUsageList;

private slots:
	void sendUsageSelected(const QModelIndex& index);

public:
	KeySelectionView(QWidget *parent = NULL);
	bool eventFilter(QObject *obj, QEvent *event);
	virtual void show();
	virtual void hideEvent(QHideEvent *e);

signals:
	void usageSelected(QString name, uint8_t usage);
	void dismissed();

public slots:
	void filterChanged(QString);
};

#endif
