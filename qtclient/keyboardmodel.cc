#include <QDebug>

#include "keyboardmodel.h"
#include "keyboardcomm.h"

#include "libusb_wrappers.h"

KeyboardModel::KeyboardModel(KeyboardComm& keyboard)
	: mLayoutID(keyboard.getLayoutID())
	, mMappingSize(keyboard.getMappingSize())
	, mNumPrograms(keyboard.getNumPrograms())
	, mProgramSpaceRaw(keyboard.getProgramSpaceRaw())
	, mProgramSpace(keyboard.getProgramSpace())
	, mMacroIndexSize(keyboard.getMacroIndexSize())
	, mMacroStorageSize(keyboard.getMacroStorageSize())
	, mMapping(keyboard.getMapping())
	, mPrograms(Program::readPrograms(keyboard.getPrograms(), keyboard.getNumPrograms()))
{
	QList<Program>& progs = mPrograms;
	for (QList<Program>::const_iterator it = progs.constBegin();
	     it != progs.constEnd();
	     ++it)
	{
		qDebug() << "Read program, length=" << it->length() <<
			" bytecode=[" << it->getByteCode() << "]";
	}
}

