#include <QList>
#include "program.h"

QList<Program> Program::readPrograms(const QByteArray& programData, int nPrograms) {
	QList<Program> programs;
	const char *p = programData.data();
	const ProgramIndex *idx =
	    reinterpret_cast<const ProgramIndex*>(p);

	for (; idx < idx + nPrograms; idx++) {
		if (idx->offset == 0xffff)
			break;

		QByteArray bytecode = QByteArray(p + idx->offset, idx->length);
		programs << Program(bytecode);
	}
	int emptySlots = nPrograms - programs.count();
	while (emptySlots--) {
		programs << Program();
	}
	return programs;
}
