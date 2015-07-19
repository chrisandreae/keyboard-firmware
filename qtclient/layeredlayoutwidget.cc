#include <QHBoxLayout>
#include <QTabBar>
#include <QDebug>

#include "layoutwidget.h"

#include "layeredlayoutwidget.h"

LayeredLayoutWidget::LayeredLayoutWidget(QWidget *parent)
	: QWidget(parent)
	, mLayout(NULL)
	, mSelectedLayer(0)
{
	LayoutWidget *layoutWidget = new LayoutWidget;
	mLayoutWidget = layoutWidget;

	QTabBar *tabBar = new QTabBar;
	mTabBar = tabBar;
	tabBar->setShape(QTabBar::RoundedEast);
	tabBar->addTab("Normal");
	tabBar->addTab("Keypad");

	QHBoxLayout *layout = new QHBoxLayout;
	layout->addWidget(layoutWidget);
	layout->addWidget(tabBar);

	connect(tabBar, SIGNAL(currentChanged(int)),
	        this, SLOT(handleTabChange(int)));

	connect(layoutWidget, SIGNAL(physicalKeyClicked(PhysicalKeycode)),
	        this, SLOT(handlePhysicalKeyClicked(PhysicalKeycode)));

	this->setLayout(layout);
}

void LayeredLayoutWidget::setScale(float f) {
	mLayoutWidget->setScale(f);
}

void LayeredLayoutWidget::setKeyboardLayout(const Layout& layout) {
	mLayout = &layout; // FIXME: aliasing without ownership
	mLayoutWidget->setKeyboardLayout(layout);
}

void LayeredLayoutWidget::setMapping(const QByteArray& mapping) {
	mMapping = mapping;
	mLayoutWidget->setMapping(this->physicalMapping());
}

QByteArray LayeredLayoutWidget::physicalMapping() const {
	return mappingForLayer(mSelectedLayer);
}

QByteArray LayeredLayoutWidget::mappingForLayer(unsigned layer) const {
	Q_ASSERT(mLayout);
	return mMapping.mid(mLayout->keys.count() * layer, mLayout->keys.count());
}

QSet<PhysicalKeycode> LayeredLayoutWidget::physicalSelection() const {
	return selectionForLayer(mSelectedLayer);
}

QSet<PhysicalKeycode> LayeredLayoutWidget::selectionForLayer(unsigned layer) const {
	Q_ASSERT(mLayout);
	const int layoutSize = mLayout->keys.count();
	const int lower = layoutSize * layer;
	const int upper = layoutSize * (layer + 1);

	QSet<PhysicalKeycode> physicalKeys;
	LogicalKeycode x;
	foreach(x, mSelection) {
		if (lower <= x && x < upper) {
			physicalKeys << x % layoutSize;
		}
	}

	return physicalKeys;
}

void LayeredLayoutWidget::setSelection(const QSet<LogicalKeycode>& selectedKeys){
	mSelection = selectedKeys;
	mLayoutWidget->setSelection(physicalSelection());
}

void LayeredLayoutWidget::setSelection(LogicalKeycode selectedKey){
	mSelection.clear();
	mSelection << selectedKey;
	mLayoutWidget->setSelection(physicalSelection());
}

void LayeredLayoutWidget::pushLayerChanges() {
	mLayoutWidget->setSelection(physicalSelection());
	mLayoutWidget->setMapping(physicalMapping());
}

void LayeredLayoutWidget::setLayer(unsigned layer) {
	mSelectedLayer = layer;
	mTabBar->setCurrentIndex(layer);
	pushLayerChanges();
}

void LayeredLayoutWidget::handleTabChange(int currentIndex) {
	mSelectedLayer = currentIndex;
	pushLayerChanges();
	emit layerSelectionChanged(currentIndex);
}


void LayeredLayoutWidget::handlePhysicalKeyClicked(PhysicalKeycode physicalKeycode) {
	emit physicalKeyClicked(physicalKeycode);
	emit logicalKeyClicked(
	    mLayout->physicalKeycodeToLogical(
	        physicalKeycode, mSelectedLayer));
}
