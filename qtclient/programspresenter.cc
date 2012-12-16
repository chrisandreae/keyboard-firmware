#include "programspresenter.h"
#include "programsitemmodel.h"
#include "keyboardmodel.h"

ProgramsPresenter::ProgramsPresenter() {
	mView = new ProgramsView(this);
}

ProgramsPresenter::~ProgramsPresenter() {
	if (!mView->parent()) {
		delete mView;
	}
}

void ProgramsPresenter::setModel(KeyboardModel *model) {
	mItemModel.reset(new ProgramsItemModel(*model->getPrograms(), model->getProgramSpace()));
	mView->setModel(mItemModel.data());
}
