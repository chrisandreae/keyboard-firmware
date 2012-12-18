// -*- c++ -*-
#ifndef LAYOUTVIEW_H
#define LAYOUTVIEW_H

#include <stdint.h>
#include <QLabel>
#include <QPixmap>

#include "layout.h"

class LayoutPresenter;
class LayoutWidget;
class KeySelectionView;

class LayoutView : public QWidget {
	Q_OBJECT
	Q_DISABLE_COPY(LayoutView)

	LayoutPresenter *mPresenter;
	LayoutWidget *mLayoutWidget;

	KeySelectionView *mKeySelectionView;

	QColor mKeypadColor;
	QColor mSelectedColor;
	bool mShowingMainLayer;

	LogicalKeycode mUpdatingLogicalKeyIndex;

public:
	LayoutView(LayoutPresenter *presenter);

	void setKeyboardLayout(const Layout *layout);

	void setMapping(const QByteArray& m);

private slots:
	void hidUsageSelected(QString name, HIDKeycode hidUsage);
	void keySelectionFinished();
	void handleLogicalKeyClicked(LogicalKeycode logicalKeycode);
};

#endif
