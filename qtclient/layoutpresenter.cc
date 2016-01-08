#include <QApplication>
#include <QDebug>

#include "hidtables.h"
#include "layoutpresenter.h"
#include "layout.h"
#include "keyboardmodel.h"

LayoutPresenter::LayoutPresenter()
	: mModel(NULL)
{
	mView = new LayoutView(this);
}

LayoutPresenter::~LayoutPresenter() {
	if (!mView->parent()) {
		delete mView;
	}
}

void LayoutPresenter::setModel(const QSharedPointer<KeyboardModel>& model) {
	mModel = model;

	mView->setKeyboardLayout(*mModel->getLayout());
	mView->setMapping(*mModel->getMapping());
}


// special keys like keypad and program must be the same on all
// layers.
static bool keyIsSpecial(HIDKeycode hidKey) {
	return (hidKey == HIDTables::HIDUsageProgram ||
	        hidKey == HIDTables::HIDUsageKeypadShift);
}

void LayoutPresenter::setHIDUsage(LogicalKeycode logicalKey, HIDKeycode hidKey) {
	QByteArray *mapping = mModel->getMapping();

	const auto& layout = *mModel->getLayout();

	PhysicalKeycode pkey =
		layout.logicalKeycodeToPhysical(logicalKey);

	// new key is special => set on all layers
	if (keyIsSpecial(hidKey)) {
		for (unsigned int layer = 0; layer < layout.layerCount(); layer++) {
			LogicalKeycode logicalKey = layout.physicalKeycodeToLogical(pkey, layer);
			(*mapping)[logicalKey] = hidKey;
		}
	}
	else {
		// old key was special => clear other layers
		if (keyIsSpecial((*mapping)[logicalKey])) {
			for (unsigned int layer = 0; layer < layout.layerCount(); layer++) {
				LogicalKeycode logicalKey = layout.physicalKeycodeToLogical(pkey, layer);
				(*mapping)[logicalKey] = HIDTables::HIDUsageNoKey;
			}
		}

		(*mapping)[logicalKey] = hidKey;
	}

	// and update the view
	mView->setMapping(*mapping);
}

void LayoutPresenter::loadDefaults() {
	QByteArray *mapping = mModel->getMapping();
	*mapping = mModel->getDefaultMapping();
	mView->setMapping(*mapping);
}
