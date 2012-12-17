#include <QApplication>
#include <QDebug>

#include "layoutpresenter.h"
#include "layout.h"
#include "keyboardmodel.h"

LayoutPresenter::LayoutPresenter()
	: mModel(NULL)
	, mShowingKeypad(false)
{
	mView = new LayoutView(this);
}

LayoutPresenter::~LayoutPresenter() {
	if (!mView->parent()) {
		delete mView;
	}
}

void LayoutPresenter::setModel(KeyboardModel *model) {
	mShowingKeypad = false;
	mModel = model;
	mLayout = &mModel->getLayout();
	mMapping = &mModel->getMapping();

	mView->setKeyboardLayout(&mModel->getLayout());
	mView->setMapping(mMapping);
}

void LayoutPresenter::setUsage(bool mainLayer, int offset, uint8_t usage) {
	QList<uint8_t>& layer = mainLayer ?
		mMapping->getMainLayer() : mMapping->getKeypadLayer();
	layer[offset] = usage;

	// since the view has a pointer directly to the model, this is
	// redundant, but used to trigger the repaint. is this poor
	// decomposition?
	mView->setMapping(mMapping);
}

void LayoutPresenter::loadDefaults() {
	mModel->getMapping().setMapping(mModel->getDefaultRawMapping());
	mView->setMapping(mMapping);
}
