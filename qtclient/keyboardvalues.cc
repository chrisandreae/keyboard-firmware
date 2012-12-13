#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>

#include "keyboardvalues.h"

static QLineEdit *newDisplay() {
	QLineEdit *lineEdit = new QLineEdit;
	lineEdit->setReadOnly(true);
	return lineEdit;
}

KeyboardValues::KeyboardValues(QWidget *parent)
	: QWidget(parent)
{
	QGridLayout *layout = new QGridLayout;
	int row;

	layout->addWidget(new QLabel("Layout ID"), row, 0);
	layout->addWidget(layoutID = newDisplay(), row, 1);
	++row;

	layout->addWidget(new QLabel("Mapping Size"), row, 0);
	layout->addWidget(mappingSize = newDisplay(), row, 1);
	++row;

	layout->addWidget(new QLabel("Num Programs"), row, 0);
	layout->addWidget(numPrograms = newDisplay(), row, 1);
	++row;

	layout->addWidget(new QLabel("Program Space (raw)"), row, 0);
	layout->addWidget(programSpaceRaw = newDisplay(), row, 1);
	++row;

	layout->addWidget(new QLabel("Program Space"), row, 0);
	layout->addWidget(programSpace = newDisplay(), row, 1);
	++row;

	layout->addWidget(new QLabel("Macro Index Size"), row, 0);
	layout->addWidget(macroIndexSize = newDisplay(), row, 1);
	++row;

	layout->addWidget(new QLabel("Macro Storage Size"), row, 0);
	layout->addWidget(macroStorageSize = newDisplay(), row, 1);
	++row;

	setLayout(layout);
}
