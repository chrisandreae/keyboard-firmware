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

class EncodedTrigger {
	QByteArray mKeys;
	int mKeysPerTrigger;
	Trigger::TriggerType mType;
	union {
		uint16_t program;
		const QByteArray* macro;
	} mData;

	// convenience function: as soon as we've used fill() to
	// appropriately size mKeys, we want to always access it as
	// a uint8_t*
	inline uint8_t* keys(){
		return reinterpret_cast<uint8_t*>(mKeys.data());
	}

	inline const uint8_t* constKeys() const {
		return reinterpret_cast<const uint8_t*>(mKeys.data());
	}

public:
	EncodedTrigger(const Trigger& t) {
		mKeysPerTrigger = t.keysPerTrigger();

		mKeys.fill(0xff, mKeysPerTrigger);

		qCopy(t.triggerKeys().constBegin(),
			  t.triggerKeys().constEnd(),
			  keys());

		mType = t.type();

		if(mType == Trigger::Program)
			mData.program = t.program();
		else
			mData.macro = &t.macro();
	}

	size_t storageRequired() const {
		return (mType == Trigger::Program) ? 0 : (2 + mData.macro->length());
	}

	bool operator< (const EncodedTrigger& other) const {
		return memcmp(constKeys(), other.constKeys(), mKeysPerTrigger) < 0;
	}

	void encode(uint8_t*& indexCursor, uint8_t*& storageCursor, const uint8_t* storageBase) const {
		memcpy(indexCursor, constKeys(), mKeysPerTrigger);
		indexCursor += mKeysPerTrigger;

		if(*constKeys() == Layout::NO_KEY){
			indexCursor += sizeof(uint16_t);
		}
		else if(mType == Trigger::Macro) {
			// write data offset to index
			writeLittleEndian<uint16_t>(indexCursor, (storageCursor - storageBase));

			// write macro body
			uint16_t macroLen = mData.macro->length();
			writeLittleEndian<uint16_t>(storageCursor, macroLen);
			memcpy(storageCursor, mData.macro->data(), macroLen);
			storageCursor += macroLen;
		}
		else {
			// write programid to index with high bit flag set
			writeLittleEndian<uint16_t>(indexCursor, mData.program | 0x8000);
		}
	}
};


QPair<QByteArray, QByteArray> Trigger::encodeTriggers(const QList<Trigger>& triggers,
													  size_t keysPerTrigger,
													  size_t indexSize,
													  size_t storageSize)
{
	// check index size
	size_t indexBytesRequired = triggers.count() * (keysPerTrigger + 2);
	if (indexBytesRequired > indexSize) {
		// Fixme: this exception message isn't very meaningful to a
		// user: better to tell them "You can only have up to 50
		// macros" than "you can only store 300 bytes of macro index"
		throw InsufficentStorageException(indexBytesRequired, indexSize, "macro index");
	}

	QList<EncodedTrigger> eTriggers;
	eTriggers.reserve(triggers.count());
	foreach (const Trigger& t, triggers){
		eTriggers << EncodedTrigger(t);
	}

	// check storage size
	size_t storageBytesRequired = 2;
	foreach (const EncodedTrigger& t, eTriggers) {
		storageBytesRequired += t.storageRequired();
	}
	if (storageBytesRequired > storageSize) {
		throw InsufficentStorageException(storageBytesRequired, storageSize, "macro storage");
	}

	// sort etriggers using operator<
	qSort(eTriggers.begin(), eTriggers.end());

	// and build the output
	QByteArray indexBytes(indexSize, 0xff);
	QByteArray storageBytes(storageBytesRequired, 0x0);

	uint8_t * const indexBase = reinterpret_cast<uint8_t*>(indexBytes.data());
	uint8_t *indexCursor = indexBase;

	uint8_t *storageBase = reinterpret_cast<uint8_t*>(storageBytes.data());
	// Prepend the end macro offset total length to the storage for the keyboard's 'next macro' cursor:
	// subtract 2 since macro offsets start after this field.
	writeLittleEndian<uint16_t>(storageBase, storageBytesRequired - 2);

	uint8_t *storageCursor = storageBase;

	// Encoding each trigger
	foreach (const EncodedTrigger& t, eTriggers) {
		t.encode(indexCursor, storageCursor, storageBase);
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
