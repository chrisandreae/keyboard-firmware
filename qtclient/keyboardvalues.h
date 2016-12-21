// -*- c++ -*-

#ifndef KEYBOARDVALUES_H
#define KEYBOARDVALUES_H

#include <QWidget>
#include <QPushButton>

class QLineEdit;
class ValuesPresenter;

class KeyboardValues : public QWidget {
	Q_OBJECT

	ValuesPresenter *mPresenter;

	typedef QLineEdit Display;
	Display *layoutID;
	Display *mappingSize;
	Display *numPrograms;
	Display *programSpaceRaw;
	Display *programSpace;
	Display *macroIndexSize;
	Display *macroStorageSize;

	QPushButton *mResetFully;

public:
	KeyboardValues(ValuesPresenter *presenter, QWidget *parent = NULL);

	void showValues(uint8_t layoutID,
	                uint8_t mappingSize,
	                uint8_t numPrograms,
	                uint16_t programSpaceRaw,
	                uint16_t programSpace,
	                uint16_t macroIndexSize,
	                uint16_t macroStorageSize);
};

#endif
