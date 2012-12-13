// -*- c++ -*-
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QStandardItemModel>
#include <QStandardItem>
#include "libusb_wrappers.h"

class KeyboardValues;

class MainWindow : public QMainWindow {
	Q_OBJECT

	QStandardItemModel *mKeyboardsModel;
	KeyboardValues *mKeyboardValues;

	static QStandardItemModel *createKeyboardItemModel();

public:
	MainWindow();

public slots:
	void keyboardSelected(int index);

};

#endif
