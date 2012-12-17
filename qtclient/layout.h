#ifndef LAYOUT_H
#define LAYOUT_H

#include <stdint.h>
#include <QRect>

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
};

#endif
