// -*- c++ -*-

#ifndef KEYBOARDVALUES_H
#define KEYBOARDVALUES_H

#include <QWidget>

class QLineEdit;

class KeyboardValues : public QWidget {
	Q_OBJECT


public:
	KeyboardValues(QWidget *parent = NULL);

	typedef QLineEdit Display;
	Display *layoutID;
	Display *mappingSize;
	Display *numPrograms;
	Display *programSpaceRaw;
	Display *programSpace;
	Display *macroIndexSize;
	Display *macroStorageSize;
};

#endif
