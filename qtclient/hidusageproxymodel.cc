#include <QDebug>

#include "hidusageproxymodel.h"
#include "hidtables.h"

bool HIDUsageProxyModel::lessThan(const QModelIndex &left,
                                  const QModelIndex &right)
	const
{
	QString leftName = sourceModel()->data(left).toString().toCaseFolded();
	uint8_t leftUsage =
		sourceModel()->data(left, HIDTables::UsageCode).toInt();

	QString rightName = sourceModel()->data(right).toString().toCaseFolded();
	uint8_t rightUsage =
		sourceModel()->data(right, HIDTables::UsageCode).toInt();

	bool leftIsPrefix = leftName.startsWith(mFilter);
	bool rightIsPrefix = rightName.startsWith(mFilter);

	if (leftName == mFilter) {
		return true;
	}
	else if (rightName == mFilter) {
		return false;
	}
	else if (leftIsPrefix ^ rightIsPrefix) {
		return leftIsPrefix;
	}
	else {
		return leftUsage < rightUsage;
	}
}
