// -*- c++ -*-
#ifndef TRIGGERSPRESENTER_H
#define TRIGGERSPRESENTER_H

#include <QObject>
#include "triggersview.h"
#include "trigger.h"

class KeyboardModel;

class TriggersPresenter : public QObject {
	Q_OBJECT

	TriggersView *mView;
	QList<Trigger>* mTriggers;

public:
	TriggersPresenter();
	~TriggersPresenter();

	QWidget *getWidget() { return mView; }

	const Trigger *getTrigger(int index);
	int getNumTriggers();
	void setTriggerType(int index, Trigger::TriggerType t);


public slots:
	void setModel(KeyboardModel *m);
};

#endif
