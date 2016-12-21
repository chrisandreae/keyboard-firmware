#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>

#include "valuespresenter.h"
#include "keyboardvalues.h"

static QLineEdit *newDisplay() {
	QLineEdit *lineEdit = new QLineEdit;
	lineEdit->setReadOnly(true);
	return lineEdit;
}

KeyboardValues::KeyboardValues(ValuesPresenter *presenter, QWidget *parent)
	: QWidget(parent)
	, mPresenter(presenter)
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

	layout->addRow(new QLabel("Full Reset"),
	               mResetFully = new QPushButton("Reset Fully"));

	connect(mResetFully, SIGNAL(clicked()),
	        mPresenter, SLOT(resetFully()));

	setLayout(layout);
}


void KeyboardValues::showValues(uint8_t layoutID,
                                uint8_t mappingSize,
                                uint8_t numPrograms,
                                uint16_t programSpaceRaw,
                                uint16_t programSpace,
                                uint16_t macroIndexSize,
                                uint16_t macroStorageSize)
{
	this->layoutID->setText(QString::number(layoutID));
	this->mappingSize->setText(QString::number(mappingSize));
	this->numPrograms->setText(QString::number(numPrograms));
	this->programSpaceRaw->setText(QString::number(programSpaceRaw));
	this->programSpace->setText(QString::number(programSpace));
	this->macroIndexSize->setText(QString::number(macroIndexSize));
	this->macroStorageSize->setText(QString::number(macroStorageSize));
}
