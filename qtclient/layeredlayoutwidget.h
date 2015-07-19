// -*- c++ -*-

#ifndef LAYEREDLAYOUTWIDGET_H
#define LAYEREDLAYOUTWIDGET_H

#include <QWidget>
#include <QSet>

#include "layout.h"

class LayoutWidget;
class QTabBar;

class LayeredLayoutWidget : public QWidget {
	Q_OBJECT;

	LayoutWidget *mLayoutWidget;
	QTabBar *mTabBar;
	const Layout *mLayout;

	QByteArray mMapping;
	QSet<LogicalKeycode> mSelection;

	unsigned mSelectedLayer;

	QByteArray physicalMapping() const;
	QSet<PhysicalKeycode> physicalSelection() const;

	QByteArray mappingForLayer(unsigned layer) const;
	QSet<PhysicalKeycode> selectionForLayer(unsigned layer) const;

	void pushLayerChanges();

public:
	LayeredLayoutWidget(QWidget* parent = NULL);

	void setScale(float f);
	void setKeyboardLayout(const Layout& layout);
	void setMapping(const QByteArray& mapping);

	void setSelection(const QSet<LogicalKeycode>& selectedKeys);
	void setSelection(LogicalKeycode selectedKey);
	void setLayer(unsigned currentLayer);

signals:
	void logicalKeyClicked (LogicalKeycode logicalKeycode);
	void physicalKeyClicked(PhysicalKeycode physicalKeycode);
	void layerSelectionChanged(unsigned currentLayer);

private slots:
	void handleTabChange(int currentIndex);
	void handlePhysicalKeyClicked(PhysicalKeycode physicalKeycode);

};

#endif
