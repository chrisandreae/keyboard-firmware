// -*- c++ -*-
#ifndef KEYSELECTIONVIEW_H
#define KEYSELECTIONVIEW_H

#include <stdint.h>
#include <QWidget>
#include "layout.h"

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
	void hidUsageSelected(QString name, HIDKeycode usage);
	void dismissed();

public slots:
	void filterChanged(QString);
};

#endif
