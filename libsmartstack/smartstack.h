#ifndef SMARTSTACK_H
#define SMARTSTACK_H

#include <string>

#include "instrumentation.h"

#define ADD_SMARTSTACK(fname)                          \
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

void printTimingReport(
    SmartStack::Stack::SortType sortType = SmartStack::Stack::SortType::Time,
    SmartStack::Stack::SortOrder sortOrder =
        SmartStack::Stack::SortOrder::Ascending) {
  SmartStack::Stack::printTimingReport(sortType, sortOrder);
}

Instrumentation addInstrumentation(const std::string &functionName,
                                   bool showStack = false) {
  return SmartStack::Instrumentation(functionName, showStack);
}

}  // namespace SmartStack

#endif  // SMARTSTACK_H
