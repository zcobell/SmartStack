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

/**
 * @brief Timer::Timer Constructor for the timer class
 */
Timer::Timer()
    : m_totalElapsed(0), m_lastElapsed(0), m_totalGlobalElapsed(0),
      m_running(false) {}

/**
 * @brief Timer::startClock Starts the timer
 */
void Timer::startClock() {
  this->m_running = true;
  this->m_startLocal = std::chrono::high_resolution_clock::now();
  this->m_startGlobal = this->m_startLocal;
}

/**
 * @brief Timer::stopClock Stops the timer
 */
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
/**
 * @brief Timer::pause Pauses the timer
 */
void Timer::pause() {
  if (this->m_running) {
    this->m_endLocal = std::chrono::high_resolution_clock::now();
    this->m_lastElapsed = std::chrono::duration_cast<std::chrono::microseconds>(
                              this->m_endLocal - this->m_startLocal)
                              .count();
    this->m_totalElapsed += this->m_lastElapsed;
  }
}

/**
 * @brief Timer::restart Restarts the timer
 */
void Timer::restart() {
  this->m_startLocal = std::chrono::high_resolution_clock::now();
}

/**
 * @brief Timer::elapsed Returns the elapsed time in microseconds
 * @return Elapsed time in microseconds
 */
size_t Timer::elapsed() const { return this->m_totalElapsed; }

/**
 * @brief Timer::globalElapsed Returns the elapsed time in microseconds
 * @return Elapsed time in microseconds
 */
size_t Timer::globalElapsed() const { return this->m_totalGlobalElapsed; }

/**
 * @brief Timer::startTime Returns the start time in milliseconds
 * @return Start time in milliseconds
 */
size_t Timer::startTime() const {
  return std::chrono::time_point_cast<std::chrono::milliseconds>(
             this->m_startLocal)
      .time_since_epoch()
      .count();
}

/**
 * @brief Timer::endTime Returns the end time in milliseconds
 * @return End time in milliseconds
 */
size_t Timer::endTime() const {
  return std::chrono::time_point_cast<std::chrono::milliseconds>(
             this->m_endLocal)
      .time_since_epoch()
      .count();
}

/**
 * @brief Timer::running Returns true if the timer is running
 * @return True if the timer is running
 */
bool Timer::running() const { return this->m_running; }

/**
 * @brief Timer::lastElapsed Returns the last elapsed time in microseconds
 * @return Last elapsed time in microseconds
 */
size_t Timer::lastElapsed() const { return this->m_lastElapsed; }
