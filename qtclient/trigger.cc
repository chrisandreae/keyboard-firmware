#include <iterator>
#include <QtEndian>
#include <QMap>
#include <QPair>
#include <QByteArray>

#include "keyboardcomm.h"
#include "trigger.h"
#include "layout.h"
#include "hidtables.h"

void Trigger::toggleKeyInTrigger(LogicalKeycode lkey){
	if(mTriggerKeys.contains(lkey)){
		mTriggerKeys.removeAll(lkey);
	}
	else{
		if(mTriggerKeys.count() < mKeysPerTrigger){
			QList<LogicalKeycode>::iterator i = qLowerBound(mTriggerKeys.begin(), mTriggerKeys.end(), lkey);
			mTriggerKeys.insert(i, lkey);
		}
	}
}

template <typename T>
static T unaligned_read(const char *p) {
	T x;
	memcpy(&x, p, sizeof(x));
	return x;
}

QList<Trigger> Trigger::readTriggers(const QByteArray& index,
                                     const QByteArray& rawData,
                                     unsigned int maxKeys)
{
	const QByteArray data =
		rawData.mid(sizeof(uint16_t)); // skip free pointer

	QList<Trigger> triggers;
	int idxOff = 0;
	while (uint8_t(index.at(idxOff)) != 0xff && idxOff < index.length()) {
		Trigger t(maxKeys);
		QList<LogicalKeycode> triggerKeys;
		for (unsigned int key = 0; key < maxKeys; key++) {
			LogicalKeycode keyValue = index.at(idxOff + key);
			if (keyValue == Layout::NO_KEY)
				break;

			QList<LogicalKeycode>::iterator i = qLowerBound(triggerKeys.begin(), triggerKeys.end(), keyValue);
			triggerKeys.insert(i, keyValue);
		}
		t.setTriggerKeys(triggerKeys);

		uint16_t dataOffset = unaligned_read<uint16_t>(
			index.constData() + idxOff + maxKeys);
		bool isProgram = !!(dataOffset & 0x8000);
		dataOffset &= 0x7fff;

		if (isProgram) {
			t.setType(Trigger::Program);
			t.setProgram(dataOffset);
		}
		else {
			t.setType(Trigger::Macro);
			uint16_t macroLength = unaligned_read<uint16_t>(
				data.constData() + dataOffset);
			t.setMacro(
				data.mid(dataOffset + sizeof(uint16_t), macroLength));
		}
		triggers << t;
		idxOff += maxKeys + sizeof(uint16_t);
	}
	return triggers;
}

template <typename T>
static void writeLittleEndian(uint8_t*& cursor, T val){
	qToLittleEndian(val, cursor);
	cursor += sizeof(val);
}


QPair<QByteArray, QByteArray> Trigger::encodeTriggers(QList<Trigger> triggers, 
													  int keysPerTrigger,
													  int indexSize,
													  int storageSize){

	// check index size
	int indexBytesRequired = triggers.count() * (keysPerTrigger + 2);
	if (indexBytesRequired > indexSize) {
		// Fixme: this exception message isn't very meaningful to a
		// user: better to tell them "You can only have up to 50
		// macros" than "you can only store 300 bytes of macro index"
		throw InsufficentStorageException(indexBytesRequired, indexSize, "macro index");
	}
	
	// check storage size
	int storageBytesRequired = 2;
	foreach (const Trigger& t, triggers) {
		if(t.type() == Trigger::Macro) {
			storageBytesRequired += 2 + t.macro().length();
		}
	}
	if (storageBytesRequired > storageSize) {
		throw InsufficentStorageException(storageBytesRequired, storageSize, "macro storage");
	}

	// Sort the triggers by their padded sorted keys
	qSort(triggers.begin(), triggers.end(), SortByTriggerKeys());

	// and build the output
	QByteArray indexBytes(indexSize, 0xff);
	QByteArray storageBytes(storageBytesRequired, 0x0);

	uint8_t * const indexBase = reinterpret_cast<uint8_t*>(indexBytes.data());
	uint8_t *indexCursor = indexBase;

	uint8_t * const storageBase = reinterpret_cast<uint8_t*>(storageBytes.data());
	uint8_t *storageCursor = storageBase;

	writeLittleEndian<uint16_t>(storageCursor, storageBytesRequired - 2);
   
	foreach (const Trigger& t, triggers) {
		// write index
		QList<LogicalKeycode> rawTrigger = t.paddedTriggerKeys();
		indexCursor = qCopy(rawTrigger.constBegin(), rawTrigger.constEnd(), indexCursor);
		
		if(rawTrigger[0] == Layout::NO_KEY) {
			// Not a valid trigger, so no relevant data: just carry on
			indexCursor += sizeof(uint16_t);
		}
		if(t.type() == Trigger::Macro) {
			// write data offset to index
			writeLittleEndian<uint16_t>(indexCursor, storageCursor - (storageBase + 2));

			// write macro body
			writeLittleEndian<uint16_t>(storageCursor, t.macro().length());
			memcpy(storageCursor, t.macro().data(), t.macro().length());
			storageCursor += t.macro().length();
		}
		else {
			// write programid to index with high bit flag set
			uint16_t programid = t.program() | 0x8000;
			writeLittleEndian<uint16_t>(indexCursor, programid);
		}
	}

	return QPair<QByteArray, QByteArray>(indexBytes, storageBytes);
}

QString Trigger::nameType(TriggerType t) {
	switch (t) {
	case Trigger::Program:
		return "Program";
	case Trigger::Macro:
		return "Macro";
	default:
		return "Invalid Type";
	}
}

QString Trigger::formatMacro(const QByteArray& macro) {
	QString formatted;
	QSet<uint8_t> downKeys;
	for (QByteArray::const_iterator it = macro.constBegin();
		 it != macro.constEnd();
		 ++it)
	{
		if (formatted.length() != 0)
			formatted += " ";

		if (downKeys.contains(*it)) {
			downKeys -= *it;
			formatted += "-";
		}
		else {
			downKeys += *it;
			formatted += "+";
		}
		formatted += HIDTables::nameUsage(*it);
	}
	return formatted;
}


