// -*- c++ -*-
#ifndef LAYOUTPRESENTER_H
#define LAYOUTPRESENTER_H

#include <stdint.h>
#include <QObject>
#include <QSharedPointer>

#include "layoutview.h"
#include "keyselectionview.h"

class KeyboardModel;
class KeySelectionView;

class LayoutPresenter : public QObject {
	Q_OBJECT

	LayoutView *mView;
	QSharedPointer<KeyboardModel> mModel;

public:
	LayoutPresenter();
	~LayoutPresenter();
	QWidget *getWidget() { return mView; }

	void setHIDUsage(LogicalKeycode logicalKey, HIDKeycode hidKey);

public slots:
	void setModel(const QSharedPointer<KeyboardModel>& model);
	void loadDefaults();
};

#endif
