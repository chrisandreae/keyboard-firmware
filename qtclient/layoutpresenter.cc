#include <QApplication>
#include <QDebug>

#include "layoutpresenter.h"
#include "layout.h"
#include "keyboardmodel.h"

LayoutPresenter::LayoutPresenter()
	: mModel(NULL)
{
	mView = new LayoutView(this);
}

LayoutPresenter::~LayoutPresenter() {
	if (!mView->parent()) {
		delete mView;
	}
}

void LayoutPresenter::setModel(KeyboardModel *model) {
	mModel = model;

	mView->setKeyboardLayout(&mModel->getLayout());
	mView->setMapping(mModel->getMapping());
}

void LayoutPresenter::setHIDUsage(LogicalKeycode logicalKey, HIDKeycode hidKey) {
	mModel->getMapping()[logicalKey] = hidKey;
	// and update the view
	mView->setMapping(mModel->getMapping());
}

void LayoutPresenter::loadDefaults() {
	mModel->getMapping() = mModel->getDefaultMapping();
	mView->setMapping(mModel->getMapping());
}
