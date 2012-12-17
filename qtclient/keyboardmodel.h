// -*- c++ -*-
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QObject>
#include "program.h"
#include "mapping.h"
#include "trigger.h"

class USBDevice;
class KeyboardComm;

class KeyboardModel {
	uint8_t mLayoutID;
	uint8_t mMappingSize;
	uint8_t mNumPrograms;
	uint16_t mProgramSpaceRaw;
	uint16_t mProgramSpace;
	uint16_t mMacroIndexSize;
	uint16_t mMacroStorageSize;

	QByteArray mDefaultMapping;
	QList<Program> mPrograms;
	QList<Trigger> mTriggers;

	// not strictly part of the model, but useful for interpreting it
	Layout mLayout;

	Mapping mMapping;

public:
	KeyboardModel(KeyboardComm& dev);

	uint8_t getLayoutID() { return mLayoutID; }
	uint8_t getMappingSize() { return mMappingSize; }
	uint8_t getNumPrograms() { return mNumPrograms; }
	uint16_t getProgramSpaceRaw() { return mProgramSpaceRaw; }
	uint16_t getProgramSpace() { return mProgramSpace; }
	uint16_t getMacroIndexSize() { return mMacroIndexSize; }
	uint16_t getMacroStorageSize() { return mMacroStorageSize; }
	QByteArray getDefaultRawMapping() { return mDefaultMapping; }
	QList<Program> *getPrograms() { return &mPrograms; }
	QList<Trigger> *getTriggers() { return &mTriggers; }
	const Layout& getLayout() { return mLayout; }
	Mapping& getMapping() { return mMapping; }
};

#endif
