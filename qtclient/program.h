// -*- c++ -*-
#ifndef PROGRAM_H
#define PROGRAM_H

#include <QByteArray>
#include <stdint.h>

// not much more than a QByteArray, still useful to have the distinct
// type.
class Program {
#if MSC_VER
#pragma pack(push, 1)
#endif
	struct ProgramIndex {
		uint16_t offset;
		uint16_t length;
	}
#if MSC_VER
#pragma pack(pop)
#endif
#ifdef __GNUC__
	__attribute__((packed))
#endif
		;

	QByteArray mByteCode;

public:
	Program() {}
	Program(const QByteArray& bytecode)
		: mByteCode(bytecode)
	{}


	static QList<Program> readPrograms(const QByteArray& programData, int nPrograms);
	static QByteArray encodePrograms(const QList<Program>& programs, int nPrograms, int maxSize);

	static QString prettyPrintInstruction(const char **p, unsigned rp);
	static QString disassemble(const QByteArray& programData);

	int length() const {
		return mByteCode.length();
	}

	QByteArray getByteCode() const {
		return mByteCode;
	}

	void setByteCode(QByteArray& bytecode) {
		mByteCode = bytecode;
	}
};

#endif
