#include <stdexcept>

#include <QXmlDefaultHandler>
#include <QXmlSimpleReader>

#include "layout.h"

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

Layout Layout::readLayout(int layoutID) {
	Layout layout;

	QString layoutXml =
		QString(":layout/%1.xml").arg(layoutID);
	XMLLayoutHandler handler(layout);
	QXmlSimpleReader xmlReader;
	QFile layoutXmlFile(layoutXml);
	QXmlInputSource source(&layoutXmlFile);
	xmlReader.setContentHandler(&handler);
	if (!xmlReader.parse(source)) {
		throw std::runtime_error("Failed to parse layout xml");
	}

	return layout;
}

QString Layout::namePosition(const LogicalKeycode logicalKeycode) const {
	PhysicalKeycode physicalKeycode;
	bool keypad = isKeypadLayer(logicalKeycode);
	if (keypad) {
		physicalKeycode = logicalKeycode - this->keypad.layerSize;
	}
	else {
		physicalKeycode = logicalKeycode;
	}

	if (physicalKeycode >= keys.count())
		return QString("Invalid Position");
	else if (!keypad)
		return keys[physicalKeycode].name;
	else
		return QString("K:%1").arg(keys[physicalKeycode].name);
}
