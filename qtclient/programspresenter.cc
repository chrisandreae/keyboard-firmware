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

void ProgramsPresenter::setModel(const QSharedPointer<KeyboardModel>& model) {
	mKeyboardModel = model;
	mView->setPrograms(model->getPrograms(), model->getProgramSpace());
}

void ProgramsPresenter::setProgram(int program, QByteArray newContents) {
	(*mKeyboardModel->getPrograms())[program].setByteCode(newContents);
	mView->programChanged(program);
}
