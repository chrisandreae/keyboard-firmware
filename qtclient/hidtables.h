#ifndef HIDTABLES_H
#define HIDTABLES_H

#include <stdint.h>
class QObject;
class QStandardItemModel;

class HIDTables {
public:
	enum HIDModelRoles {
		UsageCode = Qt::UserRole + 1
	};
	enum HIDUsages {
		HIDUsageProgram     = 0xfe,
		HIDUsageKeypadShift = 0xfc,
		HIDUsageNoKey       = 0xff,
	};
	static const char *nameUsage(uint8_t usage);
	static QStandardItemModel *newUsageModel(QObject *parent = NULL);
};

#endif
