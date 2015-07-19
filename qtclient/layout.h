// -*- c++ -*-
#ifndef LAYOUT_H
#define LAYOUT_H

#include <stdint.h>
#include <QRect>
#include <QString>
#include <QList>

typedef uint8_t PhysicalKeycode;
typedef uint8_t LogicalKeycode;
typedef uint8_t HIDKeycode;


class Layout {
public:
	static const LogicalKeycode NO_KEY = 0xff;

	struct Key {
		QString name;
		QRect rect;
	};

	QString layout;
	QString imageName;
	QList<Layout::Key> keys;

	static Layout readLayout(int layoutID);
	int mappingSize() const {
		return keys.count() * 2;
	};

	QString namePosition(const LogicalKeycode position) const;

	bool isKeypadLayer(LogicalKeycode lKey) const {
		return lKey > keys.count();
	}

	LogicalKeycode physicalKeycodeToLogical(PhysicalKeycode pKey, unsigned layer) const {
		return layer * keys.count() + pKey;
	}
	PhysicalKeycode logicalKeycodeToPhysical(LogicalKeycode lKey) const {
		return lKey % keys.count();
	}
	unsigned layerForLogicalKey(LogicalKeycode lKey) const {
		return lKey / keys.count();
	}

};

#endif
