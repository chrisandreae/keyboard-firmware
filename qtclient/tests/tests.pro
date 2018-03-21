# -*- Makefile -*-

PROJECT_ROOT = ../..
DEPENDPATH += $(PROJECT_ROOT)

QT             += core testlib
QT             -= gui
CONFIG         += moc testcase console
QMAKE_CXXFLAGS += -std=c++11

HEADERS = \
	testcompiler.h

SOURCES = \
	testcompiler.cc \
	main.cc

mac {
	CONFIG -= app_bundle
	COMPILER_PATH=../../compiler
	include(../compiler.pri)
}
