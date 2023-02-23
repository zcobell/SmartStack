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
#ifndef SMARTSTACK_INSTRUMENTATION_H
#define SMARTSTACK_INSTRUMENTATION_H

#include <string>

#include "stack.h"

namespace SmartStack {
/**
 * @brief The Instrumentation class
 *
 * This class is used to instrument a function and automatically manage
 * start/stop of the timer using variable lifetime.
 */
class Instrumentation {
 public:
  /**
   * @brief Instrumentation constructor
   * @param functionName The name of the function to instrument
   * @param showStack If true, the stack will be printed to stdout
   *
   * This constructor starts the timer for the function
   */
  explicit Instrumentation(const std::string &functionName,
                           bool showStack = false)
      : m_showStack(showStack) {
    SmartStack::Stack::startFunction(functionName, this->m_showStack);
  }

  /**
   * @brief Instrumentation destructor
   *
   * This destructor stops the timer for the function
   */
  ~Instrumentation() { SmartStack::Stack::endFunction(this->m_showStack); }

 private:
  bool m_showStack;
};
}  // namespace SmartStack

#endif  // SMARTSTACK_INSTRUMENTATION_H
