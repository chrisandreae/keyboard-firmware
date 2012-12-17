// -*- c++ -*-
#ifndef MAPPING_H
#define MAPPING_H

#include <QByteArray>
#include "layout.h"


class Mapping {
	const Layout& mLayout;
	QList<uint8_t> mMainLayer;
	QList<uint8_t> mKeypadLayer;

public:
	Mapping(const Layout& layout)
		: mLayout(layout)
	{}

	Mapping(const Layout& layout, const QByteArray& rawMapping)
		: mLayout(layout)
	{
		setMapping(rawMapping);
	}

	QList<uint8_t>& getMainLayer() { return mMainLayer; }
	QList<uint8_t>& getKeypadLayer() { return mKeypadLayer; }

	void setMapping(const QByteArray& rawMapping);
	QByteArray encodeMapping() const;
};

#endif
