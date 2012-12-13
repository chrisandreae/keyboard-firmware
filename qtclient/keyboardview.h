// -*- c++ -*-
#ifndef KEYBOARDVIEW_H
#define KEYBOARDVIEW_H

#include <QMainWindow>
#include "libusb_wrappers.h"

class KeyboardValues;
class KeyboardPresenter;
class QComboBox;
class QStringList;
class QToolButton;
class QAction;
class QStackedWidget;

class KeyboardView : public QMainWindow {
	Q_OBJECT

	KeyboardValues *mKeyboardValues;
	QComboBox *mKeyboardSelection;
	QAction *mRefreshAction;
	QStackedWidget *mWidgetStack;
    KeyboardPresenter *mPresenter;

public:
	KeyboardView(KeyboardPresenter *presenter);

	void setPresenter(KeyboardPresenter *presenter);

    void updateDevices(const QStringList& names);
	void showValues(uint8_t mLayoutID,
					uint8_t mMappingSize,
					uint8_t mNumPrograms,
					uint16_t mProgramSpaceRaw,
					uint16_t mProgramSpace,
					uint16_t mMacroIndexSize,
					uint16_t mMacroStorageSize);

	void showNoKeyboard();
	void showKeyboard();
};

#endif
