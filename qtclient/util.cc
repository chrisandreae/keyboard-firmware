#include "util.h"

#include <QModelIndex>
#include <QItemSelectionModel>

const QModelIndex currentSelectionOf(const QItemSelectionModel& selectionModel){
	if(selectionModel.hasSelection()){
		return selectionModel.selection().indexes().at(0);
	}
	else{
		return QModelIndex();
	}
}
