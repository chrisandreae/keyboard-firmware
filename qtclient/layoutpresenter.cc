#include <QApplication>
#include <QDebug>
#include <QXmlDefaultHandler>
#include <QXmlSimpleReader>

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

namespace {
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
		Q_UNUSED(namespaceURI);
		Q_UNUSED(qName);

		if (localName == "keyboard") {
			mLayout.layout = atts.value("layout");
			mLayout.imageName = atts.value("image");
		}
		else if (localName == "keypad") {
			mLayout.keypad.keyIndex = atts.value("keyindex").toInt();
			mLayout.keypad.layerStart = atts.value("layerstart").toInt();
			mLayout.keypad.layerSize = atts.value("layersize").toInt();
		}
		else if (localName == "key") {
			Layout::Key k = { atts.value("name"),
							  QRect(atts.value("x").toInt(),
									atts.value("y").toInt(),
									atts.value("w").toInt(),
									atts.value("h").toInt())
			};
			mLayout.keys.push_back(k);
		}
		return true;
	}
};
};

void LayoutPresenter::setModel(KeyboardModel *model) {
	mShowingKeypad = false;
	mModel = model;

	QString resourceDir = QString(":layout/");
	QString layoutImagePath = resourceDir + "kinesis.png";
	QString layoutXml =
		resourceDir + QString::number(model->getLayoutID()) + ".xml";

	qDebug() << "Loading layout from xml " << layoutXml;

	mLayout.reset(new Layout);
	XMLLayoutHandler handler(*mLayout);
	QXmlSimpleReader xmlReader;
	QFile layoutXmlFile(layoutXml);
	QXmlInputSource source(&layoutXmlFile);
	xmlReader.setContentHandler(&handler);
	if (!xmlReader.parse(source)) {
		qDebug() << "error reading from xml";
		return;
	}

	mMapping.reset(new Mapping(*mLayout, *model->getMapping()));
	qDebug() << "layout.imageName = " << mLayout->imageName;
	mView->setKeyboard(mLayout.data(),
	                   QPixmap(resourceDir + mLayout->imageName));
	mView->setMapping(mMapping.data());
}

void LayoutPresenter::setUsage(bool mainLayer, int offset, uint8_t usage) {
	QList<uint8_t>& layer = mainLayer ?
		mMapping->getMainLayer() : mMapping->getKeypadLayer();
	layer[offset] = usage;

	if (!mainLayer) {
		offset += mLayout->keys.count() - mLayout->keypad.layerStart;
	}
	mModel->getMapping()->data()[offset] = usage;

	// since the view has a pointer directly to the model, this is
	// redundant, but used to trigger the repaint. is this poor
	// decomposition?
	mView->setMapping(mMapping.data());
}

void LayoutPresenter::loadDefaults() {
	*mModel->getMapping() = mModel->getDefaultMapping();
	mMapping.reset(new Mapping(*mLayout, *mModel->getMapping()));
	mView->setMapping(mMapping.data());
}
