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
#ifndef INSTRUMENTATION_H
#define INSTRUMENTATION_H

#include <string>

#include "stack.h"

namespace SmartStack {
class Instrumentation {
 public:
  Instrumentation(const std::string &functionName, bool showStack = false)
      : m_showStack(showStack) {
    SmartStack::Stack::startFunction(functionName, this->m_showStack);
  }
  ~Instrumentation() { SmartStack::Stack::endFunction(this->m_showStack); }

 private:
  bool m_showStack;
};
}  // namespace SmartStack

#endif  // INSTRUMENTATION_H
