
TARGET = stack_test
CONFIG += c++11
CONFIG += console

unix {
    target.path = /usr/lib
    INSTALLS += target
}

SOURCES += \
  main.cpp

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../libsmartstack/release/ -llibsmartstack
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../libsmartstack/debug/ -llibsmartstack
else:unix: LIBS += -L$$OUT_PWD/../libsmartstack/ -llibsmartstack

INCLUDEPATH += $$PWD/../libsmartstack
DEPENDPATH += $$PWD/../libsmartstack
