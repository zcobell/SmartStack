##------------------------------GPL---------------------------------------##
## This file is part of SmartStack.
##
## (c) 2020 Zachary Cobell
##
## SmartStack is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## SmartStack is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with SmartStack.  If not, see <http://www.gnu.org/licenses/>.
##------------------------------------------------------------------------##
QT -= gui

CONFIG += c++11 console benchmark
CONFIG -= app_bundle

GOOGLE_BENCH_HOME = /opt/google/benchmark

INCLUDEPATH += $$GOOGLE_BENCH_HOME/include
LIBS += -L$$GOOGLE_BENCH_HOME/lib -lbenchmark -pthread

# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

SOURCES += main.cpp

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../libsmartstack/release/ -llibsmartstack
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../libsmartstack/debug/ -llibsmartstack
else:unix: LIBS += -L$$OUT_PWD/../libsmartstack/ -llibsmartstack

INCLUDEPATH += $$PWD/../libsmartstack
DEPENDPATH += $$PWD/../libsmartstack
