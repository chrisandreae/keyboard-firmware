#include "layoutview.h"

LayoutView::LayoutView(LayoutPresenter *presenter)
	: mPresenter(presenter)
{
}

void LayoutView::setKeyboardImage(const QPixmap& pixmap) {
	setPixmap(pixmap);
}
