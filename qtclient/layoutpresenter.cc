#include <QApplication>
#include <QDebug>
#include "layoutpresenter.h"
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

#ifdef Q_OS_MAC
	QString resourceDir = QString("%1/../../Resources/Layout/")
		.arg(QApplication::applicationDirPath());
#else
	#error This platform does not have a resource location implementation
#endif

	QString layoutImagePath = resourceDir + "kinesis.png";
	QString layoutXml =
		resourceDir + QString::number(model->getLayoutID()) + ".xml";

	qDebug() << "Switching to model " << layoutXml
			 << " image: " << layoutImagePath;

	mView->setKeyboardInfo(model->getLayoutID());
}
