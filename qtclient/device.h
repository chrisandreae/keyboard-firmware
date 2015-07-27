// -*- c++ -*-
#ifndef DEVICE_H
#define DEVICE_H

#include <exception>
#include <QString>
#include <QSharedPointer>

class DeviceSession;

class Device {
public:
	virtual QSharedPointer<DeviceSession> newSession() = 0;
	virtual QString getName() const = 0;

	virtual ~Device(){};
};

class DeviceSession {
public:
	virtual uint8_t getLayoutID() = 0;
	virtual uint8_t getMappingSize() = 0;
	virtual uint8_t getNumPrograms() = 0;
	virtual uint16_t getProgramSpaceRaw()  = 0;
	virtual uint16_t getProgramSpace()  = 0;
	virtual uint16_t getMacroIndexSize()  = 0;
	virtual uint16_t getMacroStorageSize()  = 0;
	virtual uint8_t getMacroMaxKeys()  = 0;
	virtual QByteArray getMapping() = 0;
	virtual void setMapping(const QByteArray& mapping) = 0;
	virtual QByteArray getDefaultMapping() = 0;
	virtual QByteArray getPrograms() = 0;
	virtual void setPrograms(const QByteArray& programs) = 0;
	virtual QByteArray getMacroIndex() = 0;
	virtual void setMacroIndex(const QByteArray& macroindex) = 0;
	virtual QByteArray getMacroStorage() = 0;
	virtual void setMacroStorage(const QByteArray& macroStorage) = 0;

	virtual ~DeviceSession(){};
};

class DeviceError : public std::exception {
public:
	enum Cause {
		Underflow,
	};

private:
	static const char *nameException(Cause);
	const Cause mCause;

public:
	DeviceError(Cause c)
		: mCause(c)
	{}

	virtual const char *what() const throw() {
		return nameException(mCause);
	};
};

class InsufficentStorageException : public std::exception {
private:
	const int mBytes, mAvail;
	const char * const mStorageType;
	mutable QByteArray mMessage;

public:
	InsufficentStorageException(int bytes, int avail, const char* storageType)
		: mBytes(bytes)
		, mAvail(avail)
		, mStorageType(storageType)
	{}
	~InsufficentStorageException() throw () {
	}

	virtual const char* what() const throw() {
		if (mMessage.length() == 0) {
			mMessage = QString("Cannot store %1 bytes to %2: only %3 available.")
				.arg(mBytes)
				.arg(mStorageType)
				.arg(mAvail)
				.toAscii();
		}
		return mMessage.constData();
	}
};

#endif
