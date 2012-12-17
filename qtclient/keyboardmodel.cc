#include <QDebug>

#include "keyboardmodel.h"
#include "keyboardcomm.h"
#include "trigger.h"

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
	, mDefaultMapping(keyboard.getDefaultMapping())
	, mPrograms(
	    Program::readPrograms(keyboard.getPrograms(),
	                          keyboard.getNumPrograms()))
	, mTriggers(
	    Trigger::readTriggers(keyboard.getMacroIndex(),
	                          keyboard.getMacroStorage(),
	                          keyboard.getMacroMaxKeys()))
{}
