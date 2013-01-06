// -*- c++ -*-
#ifndef PROGRAMSPRESENTER_H
#define PROGRAMSPRESENTER_H

#include <QObject>
#include <QScopedPointer>

#include "programsview.h"

class KeyboardModel;
class ProgramsItemModel;

class ProgramsPresenter : public QObject {
	Q_OBJECT

	ProgramsView *mView;
	QSharedPointer<KeyboardModel> mKeyboardModel;

public:
	ProgramsPresenter();
	~ProgramsPresenter();

	QWidget *getWidget() { return mView; }
	void setProgram(int program, QByteArray newContents);

public slots:
	void setModel(const QSharedPointer<KeyboardModel>& model);
};

#endif
