#ifndef SMARTSTACK_H
#define SMARTSTACK_H

#include <string>

#include "instrumentation.h"

#define ADD_SMARTSTACK(fname)                          \
  if (!SmartStack::sessionStarted()) {                 \
    SmartStack::startSession("NoSessionNameGiven");    \
  }                                                    \
  SmartStack::Instrumentation __SmartStackInstrument = \
      SmartStack::addInstrumentation(fname);

#define END_SMARTSTACK() SmartStack::endSession();

namespace SmartStack {

bool sessionStarted() { return SmartStack::Stack::sessionStarted(); }

void startSession(const std::string &sessionName) {
  SmartStack::Stack::startSession(sessionName);
}

void endSession() { SmartStack::Stack::endSession(); }

void printStack() { SmartStack::Stack::printCurrentStack(); }

void printTimingReport() { SmartStack::Stack::printTimingReport(); }

Instrumentation addInstrumentation(const std::string &functionName) {
  return SmartStack::Instrumentation(functionName);
}

}  // namespace SmartStack

#endif  // SMARTSTACK_H
