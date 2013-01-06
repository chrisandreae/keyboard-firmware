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

void LayoutPresenter::setModel(const QSharedPointer<KeyboardModel>& model) {
	mModel = model;

	mView->setKeyboardLayout(*mModel->getLayout());
	mView->setMapping(*mModel->getMapping());
}

void LayoutPresenter::setHIDUsage(LogicalKeycode logicalKey, HIDKeycode hidKey) {
	QByteArray *mapping = mModel->getMapping();
	(*mapping)[logicalKey] = hidKey;
	// and update the view
	mView->setMapping(*mapping);
}

void LayoutPresenter::loadDefaults() {
	QByteArray *mapping = mModel->getMapping();
	*mapping = mModel->getDefaultMapping();
	mView->setMapping(*mapping);
}
