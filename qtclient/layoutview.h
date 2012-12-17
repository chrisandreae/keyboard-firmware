// -*- c++ -*-
#ifndef LAYOUTVIEW_H
#define LAYOUTVIEW_H

#include <stdint.h>
#include <QLabel>
#include <QPixmap>

class Mapping;
class Layout;
class LayoutPresenter;
class LayoutWidget;
class KeySelectionView;

class LayoutView : public QWidget {
	Q_OBJECT
	Q_DISABLE_COPY(LayoutView)

	LayoutPresenter *mPresenter;
	LayoutWidget *mLayoutWidget;

	const Layout *mLayout;
	Mapping *mMapping;

	KeySelectionView *mKeySelectionView;

	QColor mKeypadColor;
	QColor mSelectedColor;
	bool mShowingMainLayer;

	int mUpdatingKeyIndex;

public:
	LayoutView(LayoutPresenter *presenter);

	void setKeyboardLayout(const Layout *layout);

	void setMapping(Mapping *m);

signals:
	void buttonClicked(int index, QString name);

private slots:
	void usageSelected(QString name, uint8_t usage);
	void keySelectionFinished();
	void handleKey(int keyIndex);
};

#endif
