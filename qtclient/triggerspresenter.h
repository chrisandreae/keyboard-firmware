// -*- c++ -*-
#ifndef TRIGGERSPRESENTER_H
#define TRIGGERSPRESENTER_H

#include <QObject>
#include "keyboardmodel.h"
#include "triggersview.h"
#include "trigger.h"

class KeyboardModel;

class TriggersPresenter : public QObject {
	Q_OBJECT

	TriggersView *mView;
	QList<Trigger>* mTriggers;
	QSharedPointer<KeyboardModel> mModel;

public:
	TriggersPresenter();
	~TriggersPresenter();

	QWidget *getWidget() { return mView; }

	const Trigger *getTrigger(int index) const;
	int getNumTriggers() const;
	int getKeysPerTrigger() const;
	int getNumPrograms() const;
	const Layout *getLayout() const;

	void setTriggerType(int index, Trigger::TriggerType t);
	void setTriggerProgram(int index, uint16_t program);

	void toggleKeyInTrigger(int index, LogicalKeycode logicalKeycode);

	int appendTrigger();
	void removeTrigger(int);

public slots:
	void setModel(const QSharedPointer<KeyboardModel>& m);
};

#endif
