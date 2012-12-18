// -*- c++ -*-

#ifndef TRIGGER_H
#define TRIGGER_H

#include <QList>
#include <QByteArray>
#include <QSet>

#include "layout.h";

class Trigger {
public:
	enum TriggerType {
		Macro, Program
	};

private:
	int mKeysPerTrigger;
	QSet<LogicalKeycode> mTriggerSet;

	TriggerType mType;

	QByteArray mMacroContents;
	uint16_t mProgramContents;

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

	const QSet<LogicalKeycode>& triggerSet() const { return mTriggerSet; }
	void setTriggerSet(const QSet<LogicalKeycode>& newTriggerSet) { mTriggerSet = newTriggerSet; }

	void toggleKeyInTrigger(LogicalKeycode lkey);

	static QList<Trigger> readTriggers(
	    const QByteArray& index, const QByteArray& data, unsigned int maxKeys);

	static void encodeTriggers(const QList<Trigger>& triggers);

	static QString nameType(TriggerType type);
	static QString formatMacro(const QByteArray& macro);
};

#endif
