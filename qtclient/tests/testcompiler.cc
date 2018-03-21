#include <QObject>
#include <QDebug>
#include <QTest>

#include "TestCompiler.h"

#include <HsFFI.h>
#include "LibKeyc_stub.h"


void TestCompiler::init() {
	hs_init(NULL, NULL);
}

void TestCompiler::cleanup() {
	hs_exit();
}

void TestCompiler::testCompileError() {
	size_t out_len = 0;
	char  *out = nullptr;
	size_t err_len = 0;
	char  *err = nullptr;

	int result =
	    compile(const_cast<char*>("/dev/null"),
	            &out_len, &out,
	            &err_len, &err);

	QVERIFY(result == 1);

	QVERIFY(out == nullptr);
	QVERIFY(out_len == 0);

	QVERIFY(err != nullptr);
	QVERIFY(err_len > 0);

	QVERIFY(strcmp(err, "Prelude.head: empty list") == 0);

	free(out);
	free(err);
}
