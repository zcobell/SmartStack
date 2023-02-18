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

#include "report.h"
#include <algorithm>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <string_view>

using namespace SmartStack;

Stack *Stack::instance() {
  static auto instance = std::make_unique<Stack>(5000);
  return instance.get();
}

Stack::Stack(size_t reserve)
    : m_procid(-1), m_logfile(std::string()), m_procString(std::string()),
      m_started(false), m_firstProfile(true), m_logToFile(false),
      m_proc0toScreen(false) {
  if (reserve > 0) {
    this->m_functionStack.reserve(reserve);
    this->m_functions.reserve(reserve);
    this->m_functionLookup.reserve(reserve);
  }
}

Stack::~Stack() { this->m_endSession(); }

void Stack::startSession(const std::string &session, const int &procid,
                         const bool proc0ToScreen, const std::string &logfile) {
  Stack::instance()->m_startSession(session, procid, proc0ToScreen, logfile);
}

void Stack::endSession() { Stack::instance()->m_endSession(); }

void Stack::startFunction(const std::string &functionName, bool showStack) {
  if (!Stack::instance()->m_sessionStarted()) {
    Stack::instance()->startSession("NoSessionGiven");
  }
  Stack::instance()->m_startFunction(functionName);
  if (showStack) {
    Stack::instance()->m_printCurrentStack("Enter");
  }
}

void Stack::endFunction(bool showStack) {
  if (showStack) {
    Stack::instance()->m_printCurrentStack("Return");
  }
  Stack::instance()->m_endFunction();
}

void Stack::printCurrentStack(const std::string &message) {
  Stack::instance()->m_printCurrentStack(message);
}

void Stack::printCurrentFunction(const std::string &message) {
  Stack::instance()->m_printCurrentFunction(message);
}

std::string Stack::getCurrentStack() {
  return Stack::instance()->m_getCurrentStack();
}

std::string Stack::getCurrentFunction() {
  return Stack::instance()->m_getCurrentFunction();
}

bool Stack::sessionStarted() { return Stack::instance()->m_sessionStarted(); }

bool Stack::m_sessionStarted() const { return this->m_started; }

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
  if (!logfile.empty()) {
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
  this->m_functions.emplace_back(std::make_unique<Function>(name));
  this->m_functionLookup[name] = this->m_functions.back().get();
  return this->m_functions.back().get();
}

void Stack::m_startFunction(const std::string &functionName) {
  if (!this->m_functionStack.empty()) {
    this->m_functionStack.back()->pauseFunction();
  }
  Function *f = this->getFunctionPointer(functionName);
  f->startFunction();
  this->m_functionStack.push_back(f);
}

void Stack::m_endFunction() {
  Function *f = this->m_functionStack.back();
  f->endFunction();
  this->m_functionStack.pop_back();
  if (!this->m_functionStack.empty()) {
    this->m_functionStack.back()->restartFunction();
  }
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
  if (!message.empty()) {
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
  if (!message.empty()) {
    s += ": " + message;
  }
  return s;
}

std::vector<Function *> Stack::getFunctionList() {
  std::vector<Function *> f;
  for (auto &i : Stack::instance()->m_functions) {
    f.push_back(i.get());
  }
  return f;
}

std::string Stack::sessionName() { return Stack::instance()->m_sessionName; }
