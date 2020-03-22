

#include <iostream>
#include <memory>

#include "smartstack.h"

extern "C" {
void startSessionFtn(char* sessionName);
void endSessionFtn();
void* addSmartStackFtn(char* functionName);
void deleteSmartStackFtn(void* ptr);
void printFunctionStackFtn();
void printTimingReportFtn();
}

void startSessionFtn(char* sessionName) {
  SmartStack::startSession(sessionName);
}

void endSessionFtn() {
  SmartStack::endSession();
}

void* addSmartStackFtn(char* functionName) {
  SmartStack::Instrumentation* s =
      new SmartStack::Instrumentation(functionName);
  return (void*)s;
}

void deleteSmartStackFtn(void* ptr) {
  SmartStack::Instrumentation* s =
      reinterpret_cast<SmartStack::Instrumentation*>(ptr);
  delete s;
}

void printFunctionStackFtn() { SmartStack::printStack(); }

void printTimingReportFtn() { SmartStack::printTimingReport(); }
