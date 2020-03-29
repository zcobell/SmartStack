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
#ifndef TIMER_H
#define TIMER_H

#include <chrono>

#include "smartstack_global.h"

class Timer {
 public:
  SMARTSTACK_EXPORT Timer();
  void SMARTSTACK_EXPORT startClock();
  void SMARTSTACK_EXPORT stopClock();
  void SMARTSTACK_EXPORT pause();
  void SMARTSTACK_EXPORT restart();
  long long SMARTSTACK_EXPORT elapsed() const;
  long long SMARTSTACK_EXPORT globalElapsed() const;
  long long SMARTSTACK_EXPORT startTime() const;
  long long SMARTSTACK_EXPORT endTime() const;
  long long SMARTSTACK_EXPORT lastElapsed() const;
  bool SMARTSTACK_EXPORT running() const;

 private:
  long long m_totalElapsed;
  long long m_lastElapsed;
  long long m_totalGlobalElapsed;
  bool m_running;
  std::chrono::high_resolution_clock::time_point m_startLocal;
  std::chrono::high_resolution_clock::time_point m_endLocal;
  std::chrono::high_resolution_clock::time_point m_startGlobal;
  std::chrono::high_resolution_clock::time_point m_endGlobal;
};

#endif  // TIMER_H
