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

const Trigger& TriggersPresenter::getTrigger(int index) {
	return mTriggers->at(index);
}

void TriggersPresenter::setModel(KeyboardModel *m) {
	mModel = m;
	mTriggers = m->getTriggers();
	mView->setKeyboardLayout(&m->getLayout());
	mView->triggersChanged();
}

void TriggersPresenter::setTriggerType(int index, Trigger::TriggerType t) {
	(*mTriggers)[index].setType(t);
	mView->triggerChanged(index);
}

void TriggersPresenter::toggleKeyInTrigger(int index, LogicalKeycode logicalKeycode) {
	(*mTriggers)[index].toggleKeyInTrigger(logicalKeycode);
	mView->triggerChanged(index);
}

int TriggersPresenter::getNumTriggers() {
	if (mTriggers)
		return mTriggers->count();
	else
		return 0;
}

int TriggersPresenter::getKeysPerTrigger(){
	if (mModel)
		return mModel->getKeysPerTrigger();
	else
		return 0;
}

const Layout& TriggersPresenter::getLayout() const {
	return mModel->getLayout();
}
