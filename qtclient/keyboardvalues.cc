#include <QFormLayout>
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
	QFormLayout *layout = new QFormLayout;

	layout->addRow(new QLabel("Layout ID"),
	               layoutID = newDisplay());

	layout->addRow(new QLabel("Mapping Size"),
	               mappingSize = newDisplay());

	layout->addRow(new QLabel("Num Programs"),
	               numPrograms = newDisplay());

	layout->addRow(new QLabel("Program Space (raw)"),
	               programSpaceRaw = newDisplay());

	layout->addRow(new QLabel("Program Space"),
	               programSpace = newDisplay());

	layout->addRow(new QLabel("Macro Index Size"),
	               macroIndexSize = newDisplay());

	layout->addRow(new QLabel("Macro Storage Size"),
	               macroStorageSize = newDisplay());

	setLayout(layout);
}
