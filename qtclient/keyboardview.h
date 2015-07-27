// -*- c++ -*-
#ifndef KEYBOARDVIEW_H
#define KEYBOARDVIEW_H

#include <QMainWindow>
#include <QList>
#include <QPair>
#include "libusb_wrappers.h"

class KeyboardPresenter;
class KeyboardValues;
class QComboBox;
class QStringList;
class QToolButton;
class QAction;
class QStackedWidget;

class KeyboardView : public QMainWindow {
	Q_OBJECT

	KeyboardPresenter *mPresenter;
	KeyboardValues *mKeyboardValues;
	QComboBox *mKeyboardSelection;

	QAction *mRefreshAction;
	QAction *mDownloadAction;
	QAction *mUploadAction;

	QStackedWidget *mSelectionStack;
	QStackedWidget *mSubviewStack;

public:
	KeyboardView(KeyboardPresenter *presenter,
				 QList<QPair<QString, QWidget*> > subviews);

	void updateDevices(const QStringList& names);

	void showNoKeyboard();
	void showKeyboard();
};

#endif
