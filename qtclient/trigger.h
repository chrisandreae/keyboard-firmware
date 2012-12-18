// -*- c++ -*-

#ifndef TRIGGER_H
#define TRIGGER_H

#include <QList>
#include <QByteArray>
#include <QSet>

#include "layout.h"

class Trigger {
public:
	enum TriggerType {
		Macro, Program
	};

private:
	typedef QPair<const Trigger*, QList<uint8_t> > TriggerWithKeys;

	int mKeysPerTrigger;
	QList<LogicalKeycode> mTriggerKeys;

	TriggerType mType;

	QByteArray mMacroContents;
	uint16_t mProgramContents;

	/** 
	 * Provide a padded and sorted trigger key list ready to write to
	 * device.
	 */
	const QList<LogicalKeycode> paddedTriggerKeys() const {
		QList<LogicalKeycode> keys = mTriggerKeys;
		while(keys.count() < mKeysPerTrigger)
			keys << 0xff;
		return keys;
	}

	static TriggerWithKeys pairWithKeys(const Trigger& t) {
		return TriggerWithKeys(&t, t.paddedTriggerKeys());
	}

	static bool compareKeys(const TriggerWithKeys& left, const TriggerWithKeys& right);

public:
	Trigger(int keysPerTrigger)
		: mKeysPerTrigger(keysPerTrigger)
		, mType(Macro)
		, mProgramContents(0)
	{
	}

	TriggerType type() const { return mType; };
	void setType(TriggerType type) { mType = type; }

	uint16_t program() const { return mProgramContents; }
	void setProgram(uint16_t program) { mProgramContents = program; }

	const QByteArray& macro() const { return mMacroContents; }
	void setMacro(const QByteArray& macroContents) { mMacroContents = macroContents; }

	const QList<LogicalKeycode>& triggerKeys() const { return mTriggerKeys; }
	void setTriggerKeys(const QList<LogicalKeycode>& newTriggerKeys) { mTriggerKeys = newTriggerKeys; }

	void toggleKeyInTrigger(LogicalKeycode lkey);

	static QList<Trigger> readTriggers(
	    const QByteArray& index, const QByteArray& data, unsigned int maxKeys);

	static QPair<QByteArray, QByteArray> encodeTriggers(QList<Trigger> triggers, 
														int keysPerTrigger,
														int indexSize,
														int storageSize);

	static QString nameType(TriggerType type);
	static QString formatMacro(const QByteArray& macro);
};

#endif
