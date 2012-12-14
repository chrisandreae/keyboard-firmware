#include <QApplication>
#include <QDebug>
#include <QXmlDefaultHandler>
#include <QXmlSimpleReader>

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

namespace {
class Layout {
	QString mImageName;

	friend class XMLLayoutHandler;
public:
	QString imageName() { return mImageName; }
};

class XMLLayoutHandler : public QXmlDefaultHandler {
	Layout& mLayout;
public:
	XMLLayoutHandler(Layout& layout)
		: mLayout(layout)
	{}

	virtual bool startElement(const QString& namespaceURI,
	                          const QString& localName,
	                          const QString& qName,
	                          const QXmlAttributes& atts)
	{
		if (localName == "keyboard") {
			qDebug() << "setting mImageName";
			mLayout.mImageName = atts.value("image");
		}
		return true;
	}
};
};

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

	qDebug() << "Loading layout from xml " << layoutXml;

	Layout layout;
	XMLLayoutHandler handler(layout);
	QXmlSimpleReader xmlReader;
	QFile layoutXmlFile(layoutXml);
	QXmlInputSource source(&layoutXmlFile);
	xmlReader.setContentHandler(&handler);
	if (!xmlReader.parse(source)) {
		qDebug() << "error reading from xml";
		return;
	}

	qDebug() << "layout.imageName = " << layout.imageName();
	mView->setKeyboardImage(QPixmap(resourceDir + layout.imageName()));
}
