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
#ifndef FUNCTION_H
#define FUNCTION_H

#include <string>

#include "timer.h"

namespace SmartStack::detail {

class Function {
  /**
   * @brief The Function class
   *
   * This class is used to track the timing of a function.  It is used
   * internally by the SmartStack class.
   */
 public:
  explicit Function(std::string name);

  void startFunction();
  void endFunction();

  void pauseFunction();
  void restartFunction();

  long long meanDuration();
  long long meanGlobalDuration();

  Timer *timer();

  std::string name() const;

  long long numCalls() const;

  bool running() const;

 private:
  const std::string m_name;
  long long m_ncall;
  Timer m_timer;
};
}  // namespace SmartStack::detail

#endif  // FUNCTION_H
