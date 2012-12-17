#include <stdint.h>
#include <QList>
#include <QMap>
#include <QString>
#include <QDebug>
#include "program.h"
#include "vm.h"

using namespace VM;

QList<Program> Program::readPrograms(const QByteArray& programData, int nPrograms) {
	QList<Program> programs;
	const ProgramIndex *idx =
	    reinterpret_cast<const ProgramIndex*>(programData.data());
	const ProgramIndex *idxEnd = idx + nPrograms;
	const char *p =
		reinterpret_cast<const char*>(idx + nPrograms);

	for (; idx < idxEnd; idx++) {
		if (idx->offset == 0xffff) {
			programs << Program();
		}
		else {
			QByteArray bytecode = QByteArray(p + idx->offset, idx->length);
			programs << Program(bytecode);
		}
	}
	return programs;
}

QByteArray Program::encodePrograms(const QList<Program>& programs, int nPrograms, int maxSize) {
	QByteArray encoded(maxSize, int8_t(0xff));

	ProgramIndex *idx =
	    reinterpret_cast<ProgramIndex*>(encoded.data());
	char * const dataStart =
		reinterpret_cast<char*>(idx + nPrograms);
	char *p = dataStart;
	char * const end = encoded.data() + maxSize;

	for (int i = 0; i < nPrograms; i++) {
		const QByteArray& bytecode = programs[i].getByteCode();
		uint16_t proglen = bytecode.length();
		if (bytecode.length() != 0) {
			idx[i].offset = p - dataStart;
			idx[i].length = proglen;
			if (p + proglen <= end)
				memcpy(p, bytecode.data(), proglen);
			p += proglen;
		}
		else {
			idx[i].offset = idx[i].length = 0xffff;
		}
	}

	return encoded;
}

// all instruction handling made without any assumptions to bytecode
// format, to be resilient against bytecode format changes.

static const struct { uint8_t opcode; const char *name; }
instructionNames[] = {
	{BSTORE, "BSTORE"},
	{BSTORE_0, "BSTORE_0"},
	{BSTORE_1, "BSTORE_1"},
	{BSTORE_2, "BSTORE_2"},
	{BSTORE_3, "BSTORE_3"},
	{SSTORE, "SSTORE"},
	{SSTORE_0, "SSTORE_0"},
	{SSTORE_1, "SSTORE_1"},
	{SSTORE_2, "SSTORE_2"},
	{SSTORE_3, "SSTORE_3"},
	{BLOAD, "BLOAD"},
	{BLOAD_0, "BLOAD_0"},
	{BLOAD_1, "BLOAD_1"},
	{BLOAD_2, "BLOAD_2"},
	{BLOAD_3, "BLOAD_3"},
	{SLOAD, "SLOAD"},
	{SLOAD_0, "SLOAD_0"},
	{SLOAD_1, "SLOAD_1"},
	{SLOAD_2, "SLOAD_2"},
	{SLOAD_3, "SLOAD_3"},
	{GBSTORE, "GBSTORE"},
	{GBLOAD, "GBLOAD"},
	{GSSTORE, "GSSTORE"},
	{GSLOAD, "GSLOAD"},
	{BCONST, "BCONST"},
	{BCONST_0, "BCONST_0"},
	{BCONST_1, "BCONST_1"},
	{BCONST_2, "BCONST_2"},
	{BCONST_3, "BCONST_3"},
	{SCONST, "SCONST"},
	{SCONST_0, "SCONST_0"},
	{SCONST_1, "SCONST_1"},
	{SCONST_2, "SCONST_2"},
	{SCONST_3, "SCONST_3"},
	{DUP, "DUP"},
	{DUP2, "DUP2"},
	{POP, "POP"},
	{POP2, "POP2"},
	{SWAP, "SWAP"},
	{BADD, "BADD"},
	{BSUBTRACT, "BSUBTRACT"},
	{BMULTIPLY, "BMULTIPLY"},
	{BDIVIDE, "BDIVIDE"},
	{BMOD, "BMOD"},
	{BAND, "BAND"},
	{BOR, "BOR"},
	{BXOR, "BXOR"},
	{BNOT, "BNOT"},
	{BCMP, "BCMP"},
	{BLSHIFT, "BLSHIFT"},
	{BRSHIFT, "BRSHIFT"},
	{SADD, "SADD"},
	{SSUBTRACT, "SSUBTRACT"},
	{SMULTIPLY, "SMULTIPLY"},
	{SDIVIDE, "SDIVIDE"},
	{SMOD, "SMOD"},
	{SAND, "SAND"},
	{SOR, "SOR"},
	{SXOR, "SXOR"},
	{SNOT, "SNOT"},
	{SCMP, "SCMP"},
	{SLSHIFT, "SLSHIFT"},
	{SRSHIFT, "SRSHIFT"},
	{B2S, "B2S"},
	{S2B, "S2B"},
	{IFEQ, "IFEQ"},
	{IFNE, "IFNE"},
	{IFLT, "IFLT"},
	{IFGT, "IFGT"},
	{IFGE, "IFGE"},
	{IFLE, "IFLE"},
	{GOTO, "GOTO"},
	{NOP, "NOP"},
	{CALL, "CALL"},
	{BRET, "BRET"},
	{SRET, "SRET"},
	{RET, "RET"},
	{VMEXIT, "VMEXIT"},
	{PRESSKEY, "PRESSKEY"},
	{RELEASEKEY, "RELEASEKEY"},
	{CHECKKEY, "CHECKKEY"},
	{CHECKPHYSKEY, "CHECKPHYSKEY"},
	{WAITKEY, "WAITKEY"},
	{WAITPHYSKEY, "WAITPHYSKEY"},
	{DELAY, "DELAY"},
	{GETUPTIMEMS, "GETUPTIMEMS"},
	{GETUPTIME, "GETUPTIME"},
	{BUZZ, "BUZZ"},
	{BUZZAT, "BUZZAT"},
	{MOVEMOUSE, "MOVEMOUSE"},
	{PRESSMOUSEBUTTONS, "PRESSMOUSEBUTTONS"},
	{RELEASEMOUSEBUTTONS, "RELEASEMOUSEBUTTONS"},
};

static const QString nameInstruction(uint8_t opcode) {
	static QMap<uint8_t, QString> *instructionNameMap;
	if (!instructionNameMap) {
		instructionNameMap = new QMap<uint8_t, QString>;
		for (unsigned n = 0; n < sizeof(instructionNames) / sizeof(*instructionNames); n++) {
			instructionNameMap->insert(
				instructionNames[n].opcode,
				QString(instructionNames[n].name).toLower());
		}
	}
	if (instructionNameMap->contains(opcode))
		return (*instructionNameMap)[opcode];
	else
		return "INVALID_OPCODE";
}

QString Program::prettyPrintInstruction(const char **p) {
	uint8_t opcode = *(*p)++;
	QString result = nameInstruction(opcode);
	switch (opcode) {
	case BSTORE:
	case SSTORE:
	case BLOAD:
	case SLOAD:
	case GBLOAD:
	case BCONST:
	case GBSTORE:
	case GSLOAD:
	case GSSTORE:
	case CALL:
		{
			uint8_t value = *(*p)++;
			result += " " + QString::number(value, 16);
			break;
		}
	case SCONST:
	case IFEQ:
	case IFNE:
	case IFLT:
	case IFGT:
	case IFGE:
	case IFLE:
		{
			uint16_t value;
			memcpy(&value, *p, sizeof(value));
			*p += sizeof(value);
			result += " " + QString::number(value, 16);
			break;
		}

	case GOTO:
		{
			int16_t jumpOffset;
			memcpy(&jumpOffset, *p, sizeof(jumpOffset));
			*p += sizeof(jumpOffset);
			result += " ";
			if (jumpOffset < 0) {
				jumpOffset = -jumpOffset;
				result += "-";
			}
			result += QString::number(jumpOffset, 16);
			break;
		}

	}

	return result;
}

QString Program::disassemble(const QByteArray& programData) {
	QString programDump;
	const char *p = programData.constData();
	const VM::program *programHeader =
		reinterpret_cast<const VM::program *>(p);
	const char *codeBase = reinterpret_cast<const char*>(
		&programHeader->methods[programHeader->nmethods]);

	programDump += QString("# Program: nglobals=%1 nmethods=%2\n")
		.arg(programHeader->nglobals)
		.arg(programHeader->nmethods);

	for (int mi = 0; mi < programHeader->nmethods; mi++) {
		const VM::method *methodHeader = &programHeader->methods[mi];

		programDump += QString("# method: args=%1 locals=%2 code_offset=%3\n")
			.arg(methodHeader->nargs)
			.arg(methodHeader->nlocals)
			.arg(methodHeader->code_offset);

		const char *codeEnd;
		if (mi + 1 == programHeader->nmethods) {
			codeEnd = p + programData.length();
		}
		else {
			codeEnd = codeBase + reinterpret_cast<const VM::method *>(
				&programHeader->methods[mi+1])->code_offset;
		}

		const char *codeStart = codeBase + methodHeader->code_offset;
		const char *codePtr = codeStart;
		while (codePtr < codeEnd) {
			const char *nextInstruction = codePtr;
			QString prettyInstruction = Program::prettyPrintInstruction(&nextInstruction);
			QString bytes;
			for (const char *p = codePtr; p < nextInstruction; ++p) {
				bytes += QString("%1 ").arg((uint8_t) *p, 2, 16, QLatin1Char('0'));
			}
			programDump += QString("%1: %2 %3\n")
				.arg(codePtr - codeStart, 8, 16, QLatin1Char('0'))
				.arg(bytes, -15)
				.arg(prettyInstruction);

			codePtr = nextInstruction;
		}
	}
	return programDump;
}
