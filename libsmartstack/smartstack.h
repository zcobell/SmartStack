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
#include "smartstack_global.h"

#define ADD_SMARTSTACK(fname)                          \
  SmartStack::Instrumentation __SmartStackInstrument = \
      SmartStack::addInstrumentation(fname);

#define END_SMARTSTACK() SmartStack::endSession();

namespace SmartStack {

bool SMARTSTACK_EXPORT sessionStarted() {
  return SmartStack::Stack::sessionStarted();
}

void SMARTSTACK_EXPORT startSession(const std::string &sessionName) {
  SmartStack::Stack::startSession(sessionName);
}

void SMARTSTACK_EXPORT endSession() { SmartStack::Stack::endSession(); }

void SMARTSTACK_EXPORT printStack(const std::string &message = std::string()) {
  SmartStack::Stack::printCurrentStack(message);
}

void SMARTSTACK_EXPORT printTimingReport(
    SmartStack::Stack::SortType sortType = SmartStack::Stack::SortType::Time,
    SmartStack::Stack::SortOrder sortOrder =
        SmartStack::Stack::SortOrder::Decending) {
  SmartStack::Stack::printTimingReport(sortType, sortOrder);
}

void SMARTSTACK_EXPORT saveTimingReport(
    const std::string &filename,
    SmartStack::Stack::SortType sortType = SmartStack::Stack::SortType::Time,
    SmartStack::Stack::SortOrder sortOrder =
        SmartStack::Stack::SortOrder::Decending) {
  SmartStack::Stack::saveTimingReport(filename, sortType, sortOrder);
}

Instrumentation SMARTSTACK_EXPORT
addInstrumentation(const std::string &functionName, bool showStack = false) {
  return SmartStack::Instrumentation(functionName, showStack);
}

}  // namespace SmartStack

#endif  // SMARTSTACK_H
