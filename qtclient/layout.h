// -*- c++ -*-
#ifndef LAYOUT_H
#define LAYOUT_H

#include <stdint.h>
#include <QRect>
#include <QString>
#include <QList>

class Layout {
public:
	struct Key {
		QString name;
		QRect rect;
	};

	QString layout;
	QString imageName;
	QList<Layout::Key> keys;
	struct {
		uint8_t keyIndex;
		uint8_t layerStart;
		uint8_t layerSize;
	} keypad;

	static Layout readLayout(int layoutID);
	int mappingSize() const {
		return keys.count() + keypad.layerSize ;
	};
};

#endif
