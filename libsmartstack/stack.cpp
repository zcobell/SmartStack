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
#include "stack.h"

#include <stdio.h>

#include <algorithm>
#include <fstream>
#include <iostream>

using namespace SmartStack;

bool sortFunctionTimeDecending(const std::unique_ptr<Function> &a,
                               const std::unique_ptr<Function> &b) {
  return a.get()->timer()->elapsed() > b.get()->timer()->elapsed();
}

bool sortFunctionTimeAscending(const std::unique_ptr<Function> &a,
                               const std::unique_ptr<Function> &b) {
  return a.get()->timer()->elapsed() < b.get()->timer()->elapsed();
}

bool sortFunctionCallsDecending(const std::unique_ptr<Function> &a,
                                const std::unique_ptr<Function> &b) {
  return a.get()->numCalls() > b.get()->numCalls();
}

bool sortFunctionCallsAscending(const std::unique_ptr<Function> &a,
                                const std::unique_ptr<Function> &b) {
  return a.get()->numCalls() < b.get()->numCalls();
}

bool sortFunctionMeanTimeDecending(const std::unique_ptr<Function> &a,
                                   const std::unique_ptr<Function> &b) {
  return a.get()->meanDuration() > b.get()->meanDuration();
}

bool sortFunctionMeanTimeAscending(const std::unique_ptr<Function> &a,
                                   const std::unique_ptr<Function> &b) {
  return a.get()->meanDuration() < b.get()->meanDuration();
}

bool sortFunctionMeanTotalTimeAscending(const std::unique_ptr<Function> &a,
                                        const std::unique_ptr<Function> &b) {
  return a.get()->meanGlobalDuration() < b.get()->meanGlobalDuration();
}

bool sortFunctionMeanTotalTimeDecending(const std::unique_ptr<Function> &a,
                                        const std::unique_ptr<Function> &b) {
  return a.get()->meanGlobalDuration() > b.get()->meanGlobalDuration();
}

bool sortFunctionTotalTimeAscending(const std::unique_ptr<Function> &a,
                                    const std::unique_ptr<Function> &b) {
  return a.get()->timer()->globalElapsed() < b.get()->timer()->globalElapsed();
}

bool sortFunctionTotalTimeDecending(const std::unique_ptr<Function> &a,
                                    const std::unique_ptr<Function> &b) {
  return a.get()->timer()->globalElapsed() > b.get()->timer()->globalElapsed();
}

Stack::Stack()
    : m_procid(-1),
      m_logfile(std::string()),
      m_procString(std::string()),
      m_started(false),
      m_firstProfile(true),
      m_logToFile(false),
      m_reportUnits(TimeUnits::Microseconds) {
  this->m_functionStack.reserve(5000);
  this->m_functions.reserve(5000);
  this->m_functionLookup.reserve(5000);
}

Stack::~Stack() { this->m_endSession(); }

void Stack::startSession(const std::string &session, const int &procid,
                         const bool proc0ToScreen, const std::string &logfile) {
  Stack::get().m_startSession(session, procid, proc0ToScreen, logfile);
}

void Stack::endSession() { Stack::get().m_endSession(); }

void Stack::startFunction(const std::string &functionName, bool showStack) {
  if (!Stack::get().m_sessionStarted()) {
    Stack::get().startSession("NoSessionGiven");
  }
  Stack::get().m_startFunction(functionName);
  if (showStack) {
    Stack::get().m_printCurrentStack("Enter");
  }
}

void Stack::endFunction(bool showStack) {
  if (showStack) {
    Stack::get().m_printCurrentStack("Return");
  }
  Stack::get().m_endFunction();
}

void Stack::printCurrentStack(const std::string &message) {
  Stack::get().m_printCurrentStack(message);
}

void Stack::printCurrentFunction(const std::string &message) {
  Stack::get().m_printCurrentFunction(message);
}

std::string Stack::getCurrentStack() {
  return Stack::get().m_getCurrentStack();
}

std::string Stack::getCurrentFunction() {
  return Stack::get().m_getCurrentFunction();
}

void Stack::printTimingReport(const SortType &st, const SortOrder &so) {
  std::vector<std::string> report = Stack::get().generateTimingReport(st, so);
  Stack::get().m_printTimingReport(report);
}

void Stack::saveTimingReport(const std::string &filename,
                             const Stack::SortType &st,
                             const Stack::SortOrder &so) {
  std::vector<std::string> report = Stack::get().generateTimingReport(st, so);
  Stack::get().m_saveTimimgReport(report, filename);
  return;
}

bool Stack::sessionStarted() { return Stack::get().m_sessionStarted(); }

void Stack::setReportUnits(const Stack::TimeUnits &units) {
  Stack::get().m_setReportUnits(units);
}

bool Stack::m_sessionStarted() { return this->m_started; }

void Stack::m_startSession(const std::string &session, int procid,
                           const bool proc0ToScreen,
                           const std::string &logfile) {
  this->m_sessionName = session;
  this->m_started = true;
  this->m_procid = procid;
  this->m_proc0toScreen = proc0ToScreen;

  if (this->m_procid != -1) {
    char ps[20];
    snprintf(ps, 20, "Processor %6.6d", this->m_procid);
    this->m_procString = std::string(ps);
  } else {
    this->m_procString = "";
  }
  if (logfile != std::string()) {
    this->m_logToFile = true;
    this->m_logfile = logfile;
    std::ifstream f(this->m_logfile);
    if (f.good()) {
      remove(this->m_logfile.c_str());
    }
  } else {
    this->m_logToFile = false;
    this->m_logfile = std::string();
  }
}

void Stack::m_endSession() {
  for (auto &f : this->m_functions) {
    if (f->running()) {
      f->endFunction();
    }
  }
}

Function *Stack::getFunctionPointer(const std::string &functionName) {
  auto f = this->m_functionLookup.find(functionName);
  if (f == this->m_functionLookup.end()) {
    return this->createFunction(functionName);
  } else {
    return f->second;
  }
}

Function *Stack::createFunction(const std::string &name) {
  std::unique_ptr<Function> f(new Function(name));
  this->m_functionLookup[name] = f.get();
  this->m_functions.push_back(std::move(f));
  return this->m_functions.back().get();
}

void Stack::m_startFunction(const std::string &functionName) {
  if (this->m_functionStack.size() > 0) {
    this->m_functionStack.back()->pauseFunction();
  }
  Function *f = this->getFunctionPointer(functionName);
  f->startFunction();
  this->m_functionStack.push_back(f);
  return;
}

void Stack::m_endFunction() {
  Function *f = this->m_functionStack.back();
  f->endFunction();
  this->m_functionStack.pop_back();
  if (this->m_functionStack.size() > 0) {
    this->m_functionStack.back()->restartFunction();
  }
  return;
}

void Stack::m_printCurrentStack(const std::string &message) const {
  if (this->m_logToFile) {
    std::ofstream f;
    f.open(this->m_logfile, std::ios::out | std::ios::app);
    f << this->m_getCurrentStack(message) << std::endl;
    f.flush();
    f.close();
  }

  if (this->m_procid < 0 || (this->m_procid == 0 && this->m_proc0toScreen)) {
    std::cout << this->m_getCurrentStack(message) << std::endl;
  }
}

void Stack::m_printCurrentFunction(const std::string &message) const {
  if (this->m_logToFile) {
    std::ofstream f;
    f.open(this->m_logfile, std::ios::out | std::ios::app);
    f << this->m_getCurrentFunction(message) << std::endl;
    f.flush();
    f.close();
  }

  if (this->m_procid < 0 || (this->m_procid == 0 && this->m_proc0toScreen)) {
    std::cout << this->m_getCurrentFunction(message) << std::endl;
  }
}

std::string Stack::m_getCurrentStack(const std::string &message) const {
  bool first = true;
  std::string s;
  if (this->m_procid == -1) {
    s = "[" + this->m_sessionName + "]: ";
  } else {
    s = "[" + this->m_sessionName + " " + this->m_procString + "]: ";
  }
  for (const auto &f : this->m_functionStack) {
    if (!first) {
      s += " --> " + f->name();
    } else {
      s += f->name();
      first = false;
    }
  }
  if (message != std::string()) {
    s += ": " + message;
  }
  return s;
}

std::string Stack::m_getCurrentFunction(const std::string &message) const {
  std::string s;
  if (this->m_procid == -1) {
    s = "[" + this->m_sessionName +
        "]: " + this->m_functionStack.back()->name();
  } else {
    s = "[" + this->m_sessionName + " " + this->m_procString +
        "]: " + this->m_functionStack.back()->name();
  }
  if (message != std::string()) {
    s += ": " + message;
  }
  return s;
}

void Stack::sortFunctions(const SortType &st, const SortOrder &so) {
  if (st == Time && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeAscending);
  } else if (st == Time && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeDecending);
  } else if (st == Calls && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionCallsAscending);
  } else if (st == Calls && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionCallsDecending);
  } else if (st == MeanTime && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionMeanTimeAscending);
  } else if (st == MeanTime && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionMeanTimeDecending);
  } else if (st == MeanTotalTime && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionMeanTotalTimeDecending);
  } else if (st == MeanTotalTime && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionMeanTotalTimeAscending);
  } else if (st == TotalTime && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTotalTimeDecending);
  } else if (st == TotalTime && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTotalTimeAscending);
  } else {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeDecending);
  }
  return;
}

void Stack::getSortCodes(std::string &calls, std::string &duration,
                         std::string &meanDuration, std::string &totalDuration,
                         std::string &meanTotalDuration, const SortType &st,
                         const Stack::SortOrder &so) {
  std::string upArrow = "[^]";
  std::string downArrow = "[v]";
  std::string blank = "[-]";

  if (st == SortType::Time) {
    calls = blank;
    meanDuration = blank;
    totalDuration = blank;
    meanTotalDuration = blank;
    duration = so == Ascending ? upArrow : downArrow;
  } else if (st == SortType::Calls) {
    calls = so == Ascending ? upArrow : downArrow;
    meanDuration = blank;
    duration = blank;
    totalDuration = blank;
    meanTotalDuration = blank;
  } else if (st == SortType::MeanTime) {
    calls = blank;
    totalDuration = blank;
    meanTotalDuration = blank;
    meanDuration = so == Ascending ? upArrow : downArrow;
    duration = blank;
  } else if (st == SortType::TotalTime) {
    calls = blank;
    meanDuration = blank;
    meanTotalDuration = blank;
    totalDuration = so == Ascending ? upArrow : downArrow;
    duration = blank;
  } else if (st == SortType::MeanTotalTime) {
    calls = blank;
    meanDuration = blank;
    totalDuration = blank;
    meanTotalDuration = so == Ascending ? upArrow : downArrow;
    duration = blank;
  }
  return;
}

std::vector<std::string> Stack::generateTimingReport(const SortType &st,
                                                     const SortOrder &so) {
  this->sortFunctions(st, so);
  std::string callCode, durationCode, meanDurationCode, totalDurationCode,
      meanTotalDurationCode;
  this->getSortCodes(callCode, durationCode, meanDurationCode,
                     totalDurationCode, meanTotalDurationCode, st, so);
  std::string unit = this->m_unitsString(this->m_reportUnits);

  std::vector<std::string> table;

  size_t padsize = (this->maxNumFunctionChars(13) - 13) / 2;
  std::string pad = std::string(padsize + 1, ' ');
  std::string dashfn = std::string(2 * padsize + 15, '-');
  std::string fn = pad + "Function Name" + pad;
  std::string headerbar =
      "|----------|" + dashfn +
      "|-------------|----------------------------|----------------------------"
      "---|----------------------------------|---------------------------------"
      "-----|";

  table.push_back("Function report for: " + this->m_sessionName);
  table.push_back(headerbar);
  table.push_back("|   Rank   |" + fn + "| " + callCode + " Calls   |  " +
                  durationCode + "  Local Duration  " + unit + " | " +
                  meanDurationCode + "  Mean Local Duration " + unit + " |  " +
                  totalDurationCode + " Local + Child Duration " + unit +
                  " | " + meanTotalDurationCode +
                  " Mean Local + Child Duration " + unit + " |");
  table.push_back(headerbar);

  size_t i = 0;
  for (auto &f : this->m_functions) {
    i++;
    std::string line =
        this->m_getFunctionReportLine(i, f.get(), this->m_reportUnits);
    table.push_back(line);
  }
  table.push_back(headerbar);
  return table;
}

void Stack::m_setReportUnits(const Stack::TimeUnits &units) {
  this->m_reportUnits = units;
}

std::string Stack::m_unitsString(const Stack::TimeUnits &units) const {
  switch (units) {
    case Microseconds:
      return "(us)";
    case Milliseconds:
      return "(ms)";
    case Seconds:
      return "(s) ";
    case Minutes:
      return "(m) ";
    case Hours:
      return "(h) ";
  }
}

void Stack::m_printTimingReport(const std::vector<std::string> &report) const {
  for (auto &s : report) {
    std::cout << s << std::endl;
  }
  return;
}

void Stack::m_saveTimimgReport(const std::vector<std::string> &report,
                               const std::string &filename) const {
  std::ofstream output(filename);
  for (auto &s : report) {
    output << s << std::endl;
  }
  output.close();
  return;
}

std::string Stack::m_getFunctionReportLine(
    size_t i, Function *f, const Stack::TimeUnits &units) const {
  char line[400];
  if (units == Seconds || units == Hours || units == Minutes ||
      units == Milliseconds) {
    double multiplier = 1.0;
    switch (units) {
      case Seconds:
        multiplier = 1e6;
        break;
      case Minutes:
        multiplier = 1e6 * 60.0;
        break;
      case Hours:
        multiplier = 1e6 * 60.0 * 60.0;
        break;
      case Milliseconds:
        multiplier = 1000.0;
        break;
      default:
        multiplier = 1.0;
        break;
    }

    double ts = this->convertTimeUnitsDouble(f->timer()->elapsed(), multiplier);
    double mts = this->convertTimeUnitsDouble(f->meanDuration(), multiplier);
    double ats =
        this->convertTimeUnitsDouble(f->timer()->globalElapsed(), multiplier);
    double amts =
        this->convertTimeUnitsDouble(f->meanGlobalDuration(), multiplier);

    size_t fnnamemx = this->maxNumFunctionChars(13);
    std::string formatLine =
        std::string() + "| %8zu | %" + formatStringChar(fnnamemx) +
        "s | %11lld |            %9.9e |               %9.9e |       "
        "           %9.9e |                      %9.9e |";

    snprintf(line, 400, formatLine.c_str(), i, f->name().c_str(), f->numCalls(),
             ts, mts, ats, amts);
  } else {
    size_t fnnamemx = this->maxNumFunctionChars(13);
    std::string formatLine = std::string() + "| %8zu | %" +
                             formatStringChar(fnnamemx) +
                             "s | %11lld |         %18lld |            %18lld "
                             "|               %18lld |  "
                             "                 %18lld |";
    snprintf(line, 400, formatLine.c_str(), i, f->name().c_str(), f->numCalls(),
             f->timer()->elapsed(), f->meanDuration(),
             f->timer()->globalElapsed(), f->meanGlobalDuration());
  }
  return std::string(line);
}

double Stack::convertTimeUnitsDouble(const long long time,
                                     const double multiplier) const {
  long long t = time / multiplier;
  return static_cast<double>(t) + static_cast<double>(time - t) / multiplier;
}

size_t Stack::maxNumFunctionChars(size_t lowerLimit) const {
  size_t mx = lowerLimit;
  for (auto &f : this->m_functions) {
    mx = std::max(mx, f->name().size());
  }
  return mx;
}

std::string Stack::formatStringChar(size_t n) const {
  char line[20];
  snprintf(line, 20, "%zu", n);
  return std::string(line);
}
