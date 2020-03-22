

#include <array>
#include <iostream>
#include <memory>

#include "smartstack.h"

extern "C" {
void startSessionFtn(char* sessionName);
void endSessionFtn();
void* addSmartStackFtn(char* functionName);
void deleteSmartStackFtn(void* ptr);
void printFunctionStackFtn();
void printTimingReportFtn(int sortType = 20000, int sortOrder = 10001);
}

constexpr std::array<SmartStack::Stack::SortType, 3> c_sortTypeList = {
    SmartStack::Stack::SortType::Time, SmartStack::Stack::SortType::MeanTime,
    SmartStack::Stack::SortType::Calls};

constexpr std::array<SmartStack::Stack::SortOrder, 2> c_sortOrderList = {
    SmartStack::Stack::SortOrder::Ascending,
    SmartStack::Stack::SortOrder::Decending};

void startSessionFtn(char* sessionName) {
  SmartStack::startSession(sessionName);
}

void endSessionFtn() { SmartStack::endSession(); }

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

void printTimingReportFtn(int sortType, int sortOrder) {
  SmartStack::Stack::SortType c_sortType = c_sortTypeList[sortType - 20000];
  SmartStack::Stack::SortOrder c_sortOrder = c_sortOrderList[sortOrder - 10000];
  SmartStack::printTimingReport(c_sortType, c_sortOrder);
}
