//
// Created by Zach Cobell on 2/19/23.
//
#include "smartstack.h"

bool SmartStack::sessionStarted() {
  return SmartStack::Stack::sessionStarted();
}

void SmartStack::startSession(const std::string &sessionName, const int &procid,
                              const bool proc0ToScreen,
                              const std::string &logfile) {
  SmartStack::Stack::startSession(sessionName, procid, proc0ToScreen, logfile);
}

void SmartStack::endSession() { SmartStack::Stack::endSession(); }

void SmartStack::printStack(const std::string &message) {
  SmartStack::Stack::printCurrentStack(message);
}

void SmartStack::printFunction(const std::string &message) {
  SmartStack::Stack::printCurrentFunction(message);
}

void SmartStack::printTimingReport(SmartStack::Report::TimeUnits timeUnits,
                                   SmartStack::Report::SortType sortType,
                                   SmartStack::Report::SortOrder sortOrder) {
  SmartStack::Report report(SmartStack::Stack::sessionName(), timeUnits,
                            sortType, sortOrder);
  auto functions = SmartStack::Stack::getFunctionList();
  report.printTimingReport(functions);
}

void SmartStack::saveTimingReport(const std::string &filename,
                                  SmartStack::Report::TimeUnits timeUnits,
                                  SmartStack::Report::SortType sortType,
                                  SmartStack::Report::SortOrder sortOrder,
                                  SmartStack::Report::OutputFormat format) {
  auto functions = SmartStack::Stack::getFunctionList();
  SmartStack::Report report(SmartStack::Stack::sessionName(), timeUnits,
                            sortType, sortOrder);
  if (format == SmartStack::Report::OutputFormat::Table) {
    report.saveTableTimingReport(functions, filename);
  } else if (format == SmartStack::Report::OutputFormat::CSV) {
    report.saveCsvTimingReport(functions, filename);
  } else {
    throw std::runtime_error("Invalid output format");
  }
}

std::string SmartStack::getCurrentStack() {
  return SmartStack::Stack::getCurrentStack();
}

std::string SmartStack::getCurrentFunction() {
  return SmartStack::Stack::getCurrentFunction();
}

SmartStack::Instrumentation SmartStack::addInstrumentation(
    const std::string &functionName, bool showStack) {
  return SmartStack::Instrumentation(functionName, showStack);
}