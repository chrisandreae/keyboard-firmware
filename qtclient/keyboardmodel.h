// -*- c++ -*-
#ifndef KEYBOARDMODEL_H
#define KEYBOARDMODEL_H

#include <QObject>
#include "program.h"
#include "trigger.h"
#include "layout.h"

class DeviceSession;

class KeyboardModel {
	uint8_t mLayoutID;
	uint8_t mMappingSize;
	uint8_t mNumPrograms;
	uint16_t mProgramSpaceRaw;
	uint16_t mProgramSpace;
	uint16_t mMacroIndexSize;
	uint16_t mMacroStorageSize;
	uint8_t  mKeysPerTrigger;

	QByteArray mDefaultMapping;
	QList<Program> mPrograms;
	QList<Trigger> mTriggers;

	// not strictly part of the model, but useful for interpreting it
	Layout mLayout;

	QByteArray mMapping;

public:
	KeyboardModel(DeviceSession *dev);

	uint8_t          getLayoutID()         { return mLayoutID;         }
	uint8_t          getMappingSize()      { return mMappingSize;      }
	uint8_t          getNumPrograms()      { return mNumPrograms;      }
	uint16_t         getProgramSpaceRaw()  { return mProgramSpaceRaw;  }
	uint16_t         getProgramSpace()     { return mProgramSpace;     }
	uint16_t         getMacroIndexSize()   { return mMacroIndexSize;   }
	uint16_t         getMacroStorageSize() { return mMacroStorageSize; }
	uint8_t          getKeysPerTrigger()   { return mKeysPerTrigger;   }
	QByteArray       getDefaultMapping()   { return mDefaultMapping;   }
	QList<Program> * getPrograms()         { return &mPrograms;        }
	QList<Trigger> * getTriggers()         { return &mTriggers;        }
	const Layout*    getLayout()           { return &mLayout;          }
	QByteArray*      getMapping()          { return &mMapping;         }

};

#endif
