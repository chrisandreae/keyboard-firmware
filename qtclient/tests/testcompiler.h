// -*- c++ -*-

#include <QObject>

class TestCompiler : public QObject {
	Q_OBJECT
private slots:
	void init();
	void cleanup();

	void testCompileError();
};
