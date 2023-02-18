//------------------------------GPL---------------------------------------//
// This file is part of SmartStack.
//
// (c) 2020 Zachary Cobell
//
// SmartStack is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// SmartStack is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with SmartStack.  If not, see <http://www.gnu.org/licenses/>.
//------------------------------------------------------------------------//
#ifndef SMARTSTACK_H
#define SMARTSTACK_H

#include <stdexcept>
#include <string>

#include "instrumentation.h"
#include "smartstack_global.h"

#define ADD_SMARTSTACK(fname)                                                  \
  SmartStack::Instrumentation __SmartStackInstrument =                         \
      SmartStack::addInstrumentation(fname);

#ifdef __func__
#define AUTOADD_SMARTSTACK() ADD_SMARTSTACK(__func__)
#else
#define AUTOADD_SMARTSTACK #error "__func__ not defined"
#endif

#define END_SMARTSTACK() SmartStack::endSession();

namespace SmartStack {

bool sessionStarted() { return SmartStack::Stack::sessionStarted(); }

void startSession(const std::string &sessionName, const int &procid = -1,
                  const bool proc0ToScreen = false,
                  const std::string &logfile = std::string()) {
  SmartStack::Stack::startSession(sessionName, procid, proc0ToScreen, logfile);
}

void endSession() { SmartStack::Stack::endSession(); }

void printStack(const std::string &message = std::string()) {
  SmartStack::Stack::printCurrentStack(message);
}

void printFunction(const std::string &message = std::string()) {
  SmartStack::Stack::printCurrentFunction(message);
}

void printTimingReport(
    Report::TimeUnits timeUnits = Report::TimeUnits::Milliseconds,
    Report::SortType sortType = Report::SortType::Time,
    Report::SortOrder sortOrder = Report::SortOrder::Descending) {
  Report report(Stack::sessionName(), timeUnits, sortType, sortOrder);
  auto functions = SmartStack::Stack::getFunctionList();
  report.printTimingReport(functions);
}

void saveTimingReport(
    const std::string &filename,
    Report::TimeUnits timeUnits = Report::TimeUnits::Milliseconds,
    SmartStack::Report::SortType sortType = SmartStack::Report::SortType::Time,
    SmartStack::Report::SortOrder sortOrder =
        SmartStack::Report::SortOrder::Descending,
    SmartStack::Report::OutputFormat format =
        SmartStack::Report::OutputFormat::Table) {
  auto functions = SmartStack::Stack::getFunctionList();
  Report report(Stack::sessionName(), timeUnits, sortType, sortOrder);
  if (format == SmartStack::Report::OutputFormat::Table) {
    report.saveTableTimingReport(functions, filename);
  } else if (format == SmartStack::Report::OutputFormat::CSV) {
    report.saveCsvTimingReport(functions, filename);
  } else {
    throw std::runtime_error("Invalid output format");
  }
}

std::string getCurrentStack() { return SmartStack::Stack::getCurrentStack(); }

std::string getCurrentFunction() {
  return SmartStack::Stack::getCurrentFunction();
}

Instrumentation addInstrumentation(const std::string &functionName,
                                   bool showStack = false) {
  return SmartStack::Instrumentation(functionName, showStack);
}

} // namespace SmartStack

#endif // SMARTSTACK_H
