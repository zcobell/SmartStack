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

#include <string>

#include "instrumentation.h"

#define ADD_SMARTSTACK(fname)                          \
  SmartStack::Instrumentation __SmartStackInstrument = \
      SmartStack::addInstrumentation(fname);

#define END_SMARTSTACK() SmartStack::endSession();

namespace SmartStack {

bool sessionStarted() { return SmartStack::Stack::sessionStarted(); }

void startSession(const std::string &sessionName) {
  SmartStack::Stack::startSession(sessionName);
}

void endSession() { SmartStack::Stack::endSession(); }

void printStack() { SmartStack::Stack::printCurrentStack(); }

void printTimingReport(
    SmartStack::Stack::SortType sortType = SmartStack::Stack::SortType::Time,
    SmartStack::Stack::SortOrder sortOrder =
        SmartStack::Stack::SortOrder::Decending) {
  SmartStack::Stack::printTimingReport(sortType, sortOrder);
}

Instrumentation addInstrumentation(const std::string &functionName,
                                   bool showStack = false) {
  return SmartStack::Instrumentation(functionName, showStack);
}

}  // namespace SmartStack

#endif  // SMARTSTACK_H
