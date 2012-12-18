// -*- c++ -*-

#ifndef TRIGGER_H
#define TRIGGER_H

#include <QList>
#include <QByteArray>
#include <QSet>

class Layout;

class Trigger {
public:
	enum TriggerType {
		Macro, Program
	};

private:
	QSet<uint8_t> mTriggerSet;

	QByteArray mMacroContents;
	uint16_t mProgramContents;
	TriggerType mType;

public:
	Trigger()
		: mProgramContents(0)
		, mType(Macro)
	{
	}

	TriggerType type() const { return mType; };
	void setType(TriggerType type) { mType = type; }

	uint16_t program() const { return mProgramContents; }
	void setProgram(uint16_t program) { mProgramContents = program; }

	QByteArray macro() const { return mMacroContents; }
	void setMacro(const QByteArray& macro) { mMacroContents = macro; }

	QSet<uint8_t> triggerSet() const { return mTriggerSet; };
	void setTriggerSet(const QSet<uint8_t>& triggerSet) {
		mTriggerSet = triggerSet;
	}

	static QList<Trigger> readTriggers(
	    const QByteArray& index, const QByteArray& data, unsigned int maxKeys);

	static void encodeTriggers(const QList<Trigger>& triggers);

	static QString nameType(TriggerType type);
	static QString formatMacro(const QByteArray& macro);
	static QString formatTriggerSet(const Layout& layout,
	                                const QSet<uint8_t>& macro);
};

#endif
