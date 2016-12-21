// -*- c++ -*-
#ifndef DEVICEMOCK_H
#define DEVICEMOCK_H

#include <QDebug>

#include "keyboardcomm.h"
#include "device.h"

class DeviceMock;

class DeviceSessionMock : public DeviceSession {
	DeviceMock* mDevice;
	const int mID;

	static int deviceSessionID;

public:
	DeviceSessionMock(DeviceMock *dev)
		: mDevice(dev)
		, mID(deviceSessionID++)
	{
		qDebug() << "New mock device session " << mID;
	}

	virtual ~DeviceSessionMock() {
		qDebug() << "Close mock device session " << mID;
	}

	virtual uint8_t getLayoutID() override;
	virtual uint8_t getMappingSize() override;
	virtual uint8_t getNumPrograms() override;
	virtual uint16_t getProgramSpaceRaw()  override;
	virtual uint16_t getProgramSpace()  override;
	virtual uint16_t getMacroIndexSize()  override;
	virtual uint16_t getMacroStorageSize()  override;
	virtual uint8_t getMacroMaxKeys()  override;
	virtual QByteArray getMapping() override;
	virtual void setMapping(const QByteArray& mapping) override;
	virtual QByteArray getDefaultMapping() override;
	virtual QByteArray getPrograms() override;
	virtual void setPrograms(const QByteArray& programs) override;
	virtual QByteArray getMacroIndex() override;
	virtual void setMacroIndex(const QByteArray& macroindex) override;
	virtual QByteArray getMacroStorage() override;
	virtual void setMacroStorage(const QByteArray& macroStorage) override;
	virtual void reset() override;
	virtual void resetFully() override;
};


class DeviceMock : public Device {
	QString mName;
	const uint8_t mLayoutID;
	const uint8_t mMappingSize;
	const uint8_t mNumPrograms;
	const uint16_t mRawProgramSpace;
	const uint16_t mMacroIndexSize;
	const uint16_t mMacroStorageSize;
	const uint8_t mMacroMaxKeys;
	QByteArray mMapping;
	const QByteArray mDefaultMapping;
	QByteArray mPrograms;
	QByteArray mMacroIndex;
	QByteArray mMacroStorage;

	const int mID;
	static int deviceID;

public:
DeviceMock(const QString& name,
		   uint8_t layoutID,
		   uint8_t mappingSize,
		   uint8_t numPrograms,
		   uint16_t rawProgramSpace,
		   uint16_t macroIndexSize,
		   uint16_t macroStorageSize,
		   uint8_t macroMaxKeys,
		   const QByteArray& defaultMapping)
	: mName(name),
		mLayoutID(layoutID),
		mMappingSize(mappingSize),
		mNumPrograms(numPrograms),
		mRawProgramSpace(rawProgramSpace),
		mMacroIndexSize(macroIndexSize),
		mMacroStorageSize(macroStorageSize),
		mMacroMaxKeys(macroMaxKeys),
		mDefaultMapping(defaultMapping),
		mID(deviceID++)
	{
		qDebug() << "New mock device " << mID;
	}

	virtual ~DeviceMock() {
		qDebug() << "Destroying mock device " << mID;
	}

	virtual QSharedPointer<DeviceSession> newSession() override {
		return QSharedPointer<DeviceSession>(new DeviceSessionMock(this));
	}
	virtual QString getName() const override {
		return mName;
	}

	static void enumerateTo(KeyboardComm::DeviceList* target);

	friend class DeviceSessionMock;
};


#endif
