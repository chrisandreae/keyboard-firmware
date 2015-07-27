#include "devicemock.h"
#include "keyboardcomm.h"

int DeviceSessionMock::deviceSessionID = 0;
int DeviceMock::deviceID = 0;

static const char kinesis_default_mapping[170] = {
	0
};

static const char kinesis_empty_triggers[300] = {
	(char) 0xff, 0
};

void DeviceMock::enumerateTo(KeyboardComm::DeviceList *target) {
	QSharedPointer<Device> mockKinesis = QSharedPointer<Device>(
	    new DeviceMock("Mock Kinesis",
                       1,            // layout
                       170,          // mapping size
                       6, 1024,      // programs
                       300, 1024, 4, // macros
                       QByteArray(kinesis_default_mapping, sizeof(kinesis_default_mapping))));
	{
		QSharedPointer<DeviceSession> session = mockKinesis->newSession();
		session->setMapping(QByteArray(kinesis_default_mapping, sizeof(kinesis_default_mapping)));
		session->setMacroIndex(QByteArray(kinesis_empty_triggers, sizeof(kinesis_empty_triggers)));
	}

	target->push_back(mockKinesis);
}

uint8_t DeviceSessionMock::getLayoutID() {
	return mDevice->mLayoutID;
}
uint8_t DeviceSessionMock::getMappingSize() {
	return mDevice->mMappingSize;
}
uint8_t DeviceSessionMock::getNumPrograms() {
	return mDevice->mNumPrograms;
}
uint16_t DeviceSessionMock::getProgramSpaceRaw()  {
	return mDevice->mRawProgramSpace;
}
uint16_t DeviceSessionMock::getProgramSpace()  {
	return getProgramSpaceRaw() - 4 * getNumPrograms();
}
uint16_t DeviceSessionMock::getMacroIndexSize()  {
	return mDevice->mMacroIndexSize;
}
uint16_t DeviceSessionMock::getMacroStorageSize()  {
	return mDevice->mMacroStorageSize;
}
uint8_t DeviceSessionMock::getMacroMaxKeys()  {
	return mDevice->mMacroMaxKeys;
}
QByteArray DeviceSessionMock::getMapping() {
	return mDevice->mMapping;
}
void DeviceSessionMock::setMapping(const QByteArray& mapping) {
	mDevice->mMapping = mapping;
}
QByteArray DeviceSessionMock::getDefaultMapping() {
	return mDevice->mDefaultMapping;
}
QByteArray DeviceSessionMock::getPrograms() {
	return mDevice->mPrograms;
}
void DeviceSessionMock::setPrograms(const QByteArray& programs) {
	mDevice->mPrograms = programs;
}
QByteArray DeviceSessionMock::getMacroIndex() {
	return mDevice->mMacroIndex;
}
void DeviceSessionMock::setMacroIndex(const QByteArray& macroIndex) {
	mDevice->mMacroIndex = macroIndex;
}
QByteArray DeviceSessionMock::getMacroStorage() {
	return mDevice->mMacroStorage;
}
void DeviceSessionMock::setMacroStorage(const QByteArray& macroStorage) {
	mDevice->mMacroStorage = macroStorage;
}
