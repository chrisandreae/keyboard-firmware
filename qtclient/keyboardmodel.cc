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
{
}
