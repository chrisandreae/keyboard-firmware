#include <QDebug>
#include <QMessageBox>

#include "hexdump.h"

#include "programspresenter.h"
#include "programsitemmodel.h"
#include "keyboardmodel.h"

#ifdef USE_COMPILER
#include "LibKeyc_stub.h"
#endif

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

#ifdef USE_COMPILER
void ProgramsPresenter::setProgram(int program, QString filePath) {
	/* Compile model */

	size_t codeSize = 0;
	size_t errorSize = 0;
	char *code = nullptr;
	char *error = nullptr;
	int result =
	    compile(filePath.toLatin1().data(),
	            &codeSize, &code,
	            &errorSize, &error);
	if (result == 0) {
		QByteArray newContents(code, codeSize); // deep copy
		qDebug() << "compile returned" << endl << hexdump(newContents);
		(*mKeyboardModel->getPrograms())[program].setByteCode(newContents);
		mView->programChanged(program);
	}
	else {
		qDebug() << "compile failed" << endl << error;
		QMessageBox errorMessage;
		errorMessage.setText("Compliation failed");
		errorMessage.setInformativeText(QString::fromLatin1(error, errorSize));
		errorMessage.exec();
	}

	free(code);
	free(error);
}
#else
void ProgramsPresenter::setProgram(int program, QString filePath) {
	Q_UNUSED(program);
	Q_UNUSED(filePath);
	qDebug() << "compiler not integrated; use precompiled binary";
	QMessageBox errorMessage;
	errorMessage.setText("This build of KeyboardClient does not contain a compiler.");
	errorMessage.setInformativeText(
		"Only .k files may be loaded. For .kc files, use "
		"the compiler or build with the compiler integration");
	errorMessage.exec();
}
#endif
