
TARGET = libsmartstack
TEMPLATE = lib
CONFIG += c++11
CONFIG += lib

unix {
    target.path = /usr/lib
    INSTALLS += target
}

HEADERS += \
  function.h \
  instrumentation.h \
  smartstack.h \
  stack.h \
  timer.h

SOURCES +=  \
  function.cpp \
  smartstackftn.cpp \
  stack.cpp \
  timer.cpp

