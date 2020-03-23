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

#include "smartstack_global.h"
#include "timer.h"

class Function {
 public:
  SMARTSTACK_EXPORT Function(const std::string &name);

  void SMARTSTACK_EXPORT startFunction();
  void SMARTSTACK_EXPORT endFunction();

  void SMARTSTACK_EXPORT pauseFunction();
  void SMARTSTACK_EXPORT restartFunction();

  long long SMARTSTACK_EXPORT meanDuration();

  Timer SMARTSTACK_EXPORT *timer();

  std::string SMARTSTACK_EXPORT name() const;

  long long SMARTSTACK_EXPORT numCalls() const;

  bool SMARTSTACK_EXPORT running() const;

 private:
  const std::string m_name;
  long long m_ncall;
  Timer m_timer;
};

#endif  // FUNCTION_H
