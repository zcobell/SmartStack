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
#include "timer.h"

Timer::Timer()
    : m_totalElapsed(0),
      m_lastElapsed(0),
      m_totalGlobalElapsed(0),
      m_running(false) {}

void Timer::startClock() {
  this->m_running = true;
  this->m_startLocal = std::chrono::high_resolution_clock::now();
  this->m_startGlobal = this->m_startLocal;
}

void Timer::stopClock() {
  if (this->m_running) {
    this->m_endLocal = std::chrono::high_resolution_clock::now();
    this->m_endGlobal = this->m_endLocal;
    this->m_lastElapsed = std::chrono::duration_cast<std::chrono::microseconds>(
                              this->m_endLocal - this->m_startLocal)
                              .count();
    this->m_totalElapsed += this->m_lastElapsed;
    this->m_totalGlobalElapsed +=
        std::chrono::duration_cast<std::chrono::microseconds>(
            this->m_endGlobal - this->m_startGlobal)
            .count();
    this->m_running = false;
  }
}

void Timer::pause() {
  if (this->m_running) {
    this->m_endLocal = std::chrono::high_resolution_clock::now();
    this->m_lastElapsed = std::chrono::duration_cast<std::chrono::microseconds>(
                              this->m_endLocal - this->m_startLocal)
                              .count();
    this->m_totalElapsed += this->m_lastElapsed;
  }
}

void Timer::restart() {
  this->m_startLocal = std::chrono::high_resolution_clock::now();
}

long long Timer::elapsed() const { return this->m_totalElapsed; }

long long Timer::globalElapsed() const { return this->m_totalGlobalElapsed; }

long long Timer::startTime() const {
  return std::chrono::time_point_cast<std::chrono::milliseconds>(
             this->m_startLocal)
      .time_since_epoch()
      .count();
}

long long Timer::endTime() const {
  return std::chrono::time_point_cast<std::chrono::milliseconds>(
             this->m_endLocal)
      .time_since_epoch()
      .count();
}

bool Timer::running() const { return this->m_running; }
