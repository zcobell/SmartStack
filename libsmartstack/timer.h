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

class Timer {
 public:
  Timer();
  void startClock();
  void stopClock();
  long long elapsed() const;
  long long startTime() const;
  long long endTime() const;
  long long lastElapsed() const;
  bool running() const;

 private:
  long long m_totalElapsed;
  long long m_lastElapsed;
  bool m_running;
  std::chrono::high_resolution_clock::time_point m_start;
  std::chrono::high_resolution_clock::time_point m_end;
};

#endif  // TIMER_H
