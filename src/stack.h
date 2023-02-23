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
#ifndef STACK_H
#define STACK_H

#include <memory>
#include <string>
#include <vector>

#include "function.h"
#include "report.h"

namespace SmartStack::detail {
class StackImpl;
}

namespace SmartStack::Stack {

void startSession(const std::string &session, const int &processorId = -1,
                  bool proc0ToScreen = false,
                  const std::string &logfile = std::string());
void endSession();
void startFunction(const std::string &functionName, bool showStack = false);
void endFunction(bool showStack = false);
void printCurrentStack(const std::string &message = std::string());
void printCurrentFunction(const std::string &message = std::string());
std::string getCurrentStack();
std::string getCurrentFunction();
bool sessionStarted();
void setReportUnits(const Report::TimeUnits &units);

//...Returns a static instance to the SmartStack
SmartStack::detail::StackImpl *get();

}  // namespace SmartStack::Stack

#endif  // STACK_H
