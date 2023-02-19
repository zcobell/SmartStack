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
#include "stackimpl.h"
#include <algorithm>
#include <string_view>

SmartStack::Stack::detail::StackImpl *SmartStack::Stack::detail::instance() {
  static SmartStack::Stack::detail::StackImpl instance(5000);
  return &instance;
}

void SmartStack::Stack::startSession(const std::string &session,
                                     const int &procid,
                                     const bool proc0ToScreen,
                                     const std::string &logfile) {
  SmartStack::Stack::detail::instance()->startSession(session, procid,
                                                      proc0ToScreen, logfile);
}

void SmartStack::Stack::endSession() {
  SmartStack::Stack::detail::instance()->endSession();
}

void SmartStack::Stack::startFunction(const std::string &functionName,
                                      bool showStack) {
  if (!SmartStack::Stack::detail::instance()->sessionStarted()) {
    SmartStack::Stack::detail::instance()->startSession("NoSessionGiven");
  }
  SmartStack::Stack::detail::instance()->startFunction(functionName);
  if (showStack) {
    SmartStack::Stack::detail::instance()->printCurrentStack("Enter");
  }
}

void SmartStack::Stack::endFunction(bool showStack) {
  if (showStack) {
    SmartStack::Stack::detail::instance()->printCurrentStack("Return");
  }
  SmartStack::Stack::detail::instance()->endFunction();
}

void SmartStack::Stack::printCurrentStack(const std::string &message) {
  SmartStack::Stack::detail::instance()->printCurrentStack(message);
}

void SmartStack::Stack::printCurrentFunction(const std::string &message) {
  SmartStack::Stack::detail::instance()->printCurrentFunction(message);
}

std::string SmartStack::Stack::getCurrentStack() {
  return SmartStack::Stack::detail::instance()->getCurrentStack();
}

std::string SmartStack::Stack::getCurrentFunction() {
  return SmartStack::Stack::detail::instance()->getCurrentFunction();
}

bool SmartStack::Stack::sessionStarted() {
  return SmartStack::Stack::detail::instance()->sessionStarted();
}

Function *SmartStack::Stack::getFunctionPointer(const std::string &name) {
  return SmartStack::Stack::detail::instance()->getFunctionPointer(name);
}

Function *SmartStack::Stack::createFunction(const std::string &name) {
  return SmartStack::Stack::detail::instance()->createFunction(name);
}

std::string SmartStack::Stack::sessionName() {
  return SmartStack::Stack::detail::instance()->sessionName();
}

std::vector<Function *> SmartStack::Stack::getFunctionList() {
  std::vector<Function *> f;
  for (auto &i : SmartStack::Stack::detail::instance()->functions()) {
    f.push_back(i.get());
  }
  return f;
}