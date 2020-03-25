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

Stack::Stack() : m_started(false), m_firstProfile(true) {
  this->m_functionStack.reserve(5000);
  this->m_functions.reserve(5000);
  this->m_functionLookup.reserve(5000);
}

Stack::~Stack() { this->m_endSession(); }

void Stack::startSession(const std::string &session) {
  Stack::get().m_startSession(session);
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

bool Stack::m_sessionStarted() { return this->m_started; }

void Stack::m_startSession(const std::string &session) {
  this->m_sessionName = session;
  this->m_started = true;
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
#ifndef SMARTSTACK_AGGREGATE
  if (this->m_functionStack.size() > 0) {
    this->m_functionStack.back()->pauseFunction();
  }
#endif
  Function *f = this->getFunctionPointer(functionName);
  f->startFunction();
  this->m_functionStack.push_back(f);
  return;
}

void Stack::m_printCurrentFunction(const std::string &message) {
  std::cout << "[Stack " << this->m_sessionName
            << "]: " << this->m_functionStack.back()->name() << ": " << message
            << std::endl;
}

void Stack::m_endFunction() {
  Function *f = this->m_functionStack.back();
  f->endFunction();
  this->m_functionStack.pop_back();
#ifndef SMARTSTACK_AGGREGATE
  if (this->m_functionStack.size() > 0) {
    this->m_functionStack.back()->restartFunction();
  }
#endif
  return;
}

void Stack::m_printCurrentStack(const std::string &message) {
  bool first = true;
  std::cout << "[Stack " << this->m_sessionName << "]: ";
  for (const auto &f : this->m_functionStack) {
    if (!first) {
      std::cout << " --> " << f->name();
    } else {
      std::cout << f->name();
      first = false;
    }
  }
  if (message != std::string()) {
    std::cout << ": " << message;
  }
  std::cout << std::endl;
  std::cout.flush();
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
  } else {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeDecending);
  }
  return;
}

void Stack::getSortCodes(std::string &calls, std::string &duration,
                         std::string &meanDuration, const SortType &st,
                         const Stack::SortOrder &so) {
  std::string upArrow = "[^]";
  std::string downArrow = "[v]";
  std::string blank = "[-]";

  if (st == SortType::Time) {
    calls = blank;
    meanDuration = blank;
    duration = so == Ascending ? upArrow : downArrow;
  } else if (st == SortType::Calls) {
    calls = so == Ascending ? upArrow : downArrow;
    meanDuration = blank;
    duration = blank;
  } else if (st == SortType::MeanTime) {
    calls = blank;
    meanDuration = so == Ascending ? upArrow : downArrow;
    duration = blank;
  }
  return;
}

std::vector<std::string> Stack::generateTimingReport(const SortType &st,
                                                     const SortOrder &so) {
  this->sortFunctions(st, so);
  std::string callCode, durationCode, meanDurationCode;
  this->getSortCodes(callCode, durationCode, meanDurationCode, st, so);

  std::vector<std::string> table;
  table.push_back("Function report for: " + this->m_sessionName);
  table.push_back(
      "|----------|--------------------------------|-------------|-------------"
      "-------|-----------------------|");
  table.push_back("|   Rank   |         Function Name          | " + callCode +
                  " Calls   |  " + durationCode + " Duration (s)  | " +
                  meanDurationCode + " Mean Duration (s) |");
  table.push_back(
      "|----------|--------------------------------|-------------|-------------"
      "-------|-----------------------|");

  size_t i = 0;
  for (auto &f : this->m_functions) {
    i++;
    long long t = f->timer()->elapsed() / 1e6;
    double ts = static_cast<double>(t) +
                static_cast<double>(f->timer()->elapsed() - t) / 1e6;
    long long mt = f->meanDuration() / 1e6;
    double mts = static_cast<double>(mt) +
                 static_cast<double>(f->meanDuration() - mt) / 1e6;
    char line[200];
    snprintf(line, 200,
             "| %8zu | %30s | %11lld |       %6.6e |          %6.6e |", i,
             f->name().c_str(), f->numCalls(), ts, mts);
    table.push_back(line);
  }
  table.push_back(
      "|----------|--------------------------------|-------------|-------------"
      "-------|-----------------------|");
  return table;
}

void Stack::m_printTimingReport(const std::vector<std::string> &report) {
  for (auto &s : report) {
    std::cout << s << std::endl;
  }
  return;
}

void Stack::m_saveTimimgReport(const std::vector<std::string> &report,
                               const std::string &filename) {
  std::ofstream output(filename);
  for (auto &s : report) {
    output << s << std::endl;
  }
  output.close();
  return;
}
