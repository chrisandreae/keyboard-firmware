// -*- c++ -*-
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QObject>

class USBDevice;
class KeyboardComm;

class KeyboardModel : public QObject {
	uint8_t mLayoutID;
	uint8_t mMappingSize;
	uint8_t mNumPrograms;
	uint16_t mProgramSpaceRaw;
	uint16_t mProgramSpace;
	uint16_t mMacroIndexSize;
	uint16_t mMacroStorageSize;

public:
	KeyboardModel(KeyboardComm& dev);

	uint8_t getLayoutID() { return mLayoutID; }
	uint8_t getMappingSize() { return mMappingSize; }
	uint8_t getNumPrograms() { return mNumPrograms; }
	uint16_t getProgramSpaceRaw() { return mProgramSpaceRaw; }
	uint16_t getProgramSpace() { return mProgramSpace; }
	uint16_t getMacroIndexSize() { return mMacroIndexSize; }
	uint16_t getMacroStorageSize() { return mMacroStorageSize; }	 
};

#endif
