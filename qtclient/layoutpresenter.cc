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


/* Call given function with each logical keycode corresponding to the
 * physical keycode in every layer. */
template <class UnaryFunction>
static void forEachLogicalKey(const Layout& layout, PhysicalKeycode pkey, UnaryFunction f) {
	unsigned int layer = 0;
	LogicalKeycode logicalKey;
	while ((logicalKey = layout.physicalKeycodeToLogical(pkey, layer++)) <
	       layout.mappingSize())
	{
		f(logicalKey);
	}
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
		forEachLogicalKey(layout, pkey, [&](LogicalKeycode logicalKey){
			(*mapping)[logicalKey] = hidKey;
		});
	}
	else {
		// old key was special => clear other layers
		if (keyIsSpecial((*mapping)[logicalKey])) {
			forEachLogicalKey(layout, pkey, [&](LogicalKeycode logicalKey){
				(*mapping)[logicalKey] = HIDTables::HIDUsageNoKey;
			});
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
