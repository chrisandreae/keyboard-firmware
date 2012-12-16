// -*- c++ -*-
#ifndef PROGRAM_H
#define PROGRAM_H

#include <QByteArray>

// not much more than a QByteArray, still useful to have the distinct
// type.
class Program {
	struct ProgramIndex {
		uint16_t offset;
		uint16_t length;
	} __attribute__((packed));

	QByteArray mByteCode;

public:
	Program() {}
	Program(const QByteArray& bytecode)
		: mByteCode(bytecode)
	{}


	static QList<Program> readPrograms(const QByteArray& programData, int nPrograms);
	QByteArray encodePrograms();

	int length() const {
		return mByteCode.length();
	}

	QByteArray getByteCode() const {
		return mByteCode;
	}
};

#endif
