// -*- c++ -*-
#ifndef LAYOUTVIEW_H
#define LAYOUTVIEW_H

#include <QLabel>
#include <QPixmap>

class Mapping;
class Layout;
class LayoutPresenter;
class KeySelectionView;

class LayoutView : public QLabel {
	Q_OBJECT
	Q_DISABLE_COPY(LayoutView)

	LayoutPresenter *mPresenter;

	const Layout *mLayout;
	Mapping *mMapping;

	KeySelectionView *mKeySelectionView;

	QColor mKeypadColor;
	QColor mSelectedColor;
	bool mShowingMainLayer;

	int mUpdatingKeyIndex;

public:
	LayoutView(LayoutPresenter *presenter);

	void setKeyboard(const Layout *layout, const QPixmap& pixmap);

	void setMapping(Mapping *m);
	virtual void paintEvent(QPaintEvent* e);
	virtual void mousePressEvent(QMouseEvent* e);

signals:
	void buttonClicked(int index, QString name);

private slots:
	void usageSelected(QString name, uint8_t usage);
};

#endif
