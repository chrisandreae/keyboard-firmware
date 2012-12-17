#include <QDebug>
#include <QTableView>

#include "triggerspresenter.h"
#include "triggersview.h"
#include "trigger.h"
#include "keyboardmodel.h"

TriggersPresenter::TriggersPresenter()
	: mTriggers(NULL)
{
	mView = new TriggersView(this);
}

TriggersPresenter::~TriggersPresenter() {
	if (!mView->parent()) {
		delete mView;
	}
}

const Trigger* TriggersPresenter::getTrigger(int index) {
	return &mTriggers->at(index);
}

void TriggersPresenter::setModel(KeyboardModel *m) {
	mTriggers = m->getTriggers();
	mView->triggersChanged();
}

void TriggersPresenter::setTriggerType(int index, Trigger::TriggerType t) {
	(*mTriggers)[index].setType(t);
	mView->triggerChanged(index);
}

int TriggersPresenter::getNumTriggers() {
	if (mTriggers)
		return mTriggers->count();
	else
		return 0;
}
