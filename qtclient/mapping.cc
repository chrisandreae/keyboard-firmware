#include <QList>
#include <QtAlgorithms>

#include "mapping.h"

void Mapping::setMapping(const QByteArray& rawMapping)
{
	mMainLayer.clear();
	mKeypadLayer.clear();
	int i = 0;
	for (QList<Layout::Key>::const_iterator it = mLayout.keys.constBegin();
		 it != mLayout.keys.constEnd();
		 ++it, ++i)
	{
		mMainLayer.push_back(rawMapping.at(i));
	}

	mKeypadLayer = mMainLayer;

	QList<Layout::Key>::const_iterator keypadStart =
		mLayout.keys.constBegin() + mLayout.keypad.layerStart;
	QList<Layout::Key>::const_iterator keypadEnd =
		keypadStart + mLayout.keypad.layerSize;

	for (QList<Layout::Key>::const_iterator it = keypadStart;
		 it != keypadEnd;
		 ++it, ++i)
	{
		mKeypadLayer[it - keypadStart + mLayout.keypad.layerStart]
			= rawMapping.at(i);
	}
}

QByteArray Mapping::encodeMapping() const {
	QByteArray result(mLayout.mappingSize(), 0);
	qCopy(mMainLayer.constBegin(), mMainLayer.constEnd(),
		  result.begin());
	qCopy(mKeypadLayer.constBegin() + mLayout.keypad.layerStart,
		  mKeypadLayer.constEnd(),
		  result.begin() + mMainLayer.count());
	return result;
}
