// -*- c++ -*-
#ifndef MAPPING_H
#define MAPPING_H

#include <QByteArray>
#include "layout.h"


class Mapping {
	QList<uint8_t> mMainLayer;
	QList<uint8_t> mKeypadLayer;

public:
	Mapping(const Layout& layout, const QByteArray& rawMapping) {
		int i = 0;
		for (QList<Layout::Key>::const_iterator it = layout.keys.constBegin();
			 it != layout.keys.constEnd();
			 ++it, ++i)
		{
			mMainLayer.push_back(rawMapping.at(i));
		}

		mKeypadLayer = mMainLayer;

		QList<Layout::Key>::const_iterator keypadStart =
			layout.keys.constBegin() + layout.keypad.layerStart;
		QList<Layout::Key>::const_iterator keypadEnd =
			keypadStart + layout.keypad.layerSize;

		for (QList<Layout::Key>::const_iterator it = keypadStart;
			 it != keypadEnd;
			 ++it, ++i)
		{
			mKeypadLayer[it - keypadStart + layout.keypad.layerStart]
				= rawMapping.at(i);
		}
	}

	QList<uint8_t>& getMainLayer() { return mMainLayer; }
	QList<uint8_t>& getKeypadLayer() { return mKeypadLayer; }
};

#endif
