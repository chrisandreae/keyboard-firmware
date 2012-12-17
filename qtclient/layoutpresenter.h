// -*- c++ -*-
#ifndef LAYOUTPRESENTER_H
#define LAYOUTPRESENTER_H

#include <stdint.h>
#include <QObject>
#include <QScopedPointer>

#include "layoutview.h"
#include "keyselectionview.h"

class KeyboardModel;
class KeySelectionView;

class LayoutPresenter : public QObject {
	Q_OBJECT

	LayoutView *mView;
	KeyboardModel *mModel;
	QScopedPointer<Layout> mLayout;
	QScopedPointer<Mapping> mMapping;

	QScopedPointer<KeySelectionView> mKeySelectionView;
	bool mShowingKeypad;
public:
	LayoutPresenter();
	~LayoutPresenter();
	QWidget *getWidget() { return mView; }

	void setUsage(bool mainLayer, int offset, uint8_t usage);

public slots:
	void setModel(KeyboardModel *model);
	void loadDefaults();
};

#endif
