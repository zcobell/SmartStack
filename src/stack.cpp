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
#include <iostream>

#include "stackimpl.h"

using namespace SmartStack;

SmartStack::detail::StackImpl *Stack::get() {
  static detail::StackImpl instance;
  return &instance;
}

void Stack::startSession(const std::string &session, const int &procid,
                         const bool proc0ToScreen, const std::string &logfile) {
  Stack::get()->startSession(session, procid, proc0ToScreen, logfile);
}

void Stack::endSession() { Stack::get()->endSession(); }

void Stack::startFunction(const std::string &functionName, bool showStack) {
  if (!Stack::get()->sessionRunning()) {
    Stack::get()->startSession("NoSessionGiven");
  }
  Stack::get()->startFunction(functionName);
  if (showStack) {
    Stack::get()->printCurrentStack("Enter");
  }
}

void Stack::endFunction(bool showStack) {
  if (Stack::get()->sessionRunning()) {
    if (showStack) {
      Stack::get()->printCurrentStack("Return");
    }
    Stack::get()->endFunction();
  }
}

void Stack::printCurrentStack(const std::string &message) {
  Stack::get()->printCurrentStack(message);
}

void Stack::printCurrentFunction(const std::string &message) {
  Stack::get()->printCurrentFunction(message);
}

std::string Stack::getCurrentStack() { return Stack::get()->getCurrentStack(); }

std::string Stack::getCurrentFunction() {
  return Stack::get()->getCurrentFunction();
}

bool Stack::sessionStarted() { return Stack::get()->sessionRunning(); }
