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

using namespace SmartStack::detail;

/* @brief Constructor
 *
 * This constructor initializes the timer
 */
Timer::Timer()
    : m_totalElapsed(0),
      m_lastElapsed(0),
      m_totalGlobalElapsed(0),
      m_running(false) {}

/* @brief Start the timer
 *
 * This function starts the timer.
 */
void Timer::startClock() {
  this->m_running = true;
  this->m_startLocal = std::chrono::high_resolution_clock::now();
  this->m_startGlobal = this->m_startLocal;
}

/* @brief Stop the timer
 *
 * This function stops the timer and updates the elapsed times
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

/* @brief Pause the timer
 *
 * This function pauses the timer and updates the elapsed times
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

/* @brief Restart the timer
 *
 * This function restarts the timer
 */
void Timer::restart() {
  this->m_startLocal = std::chrono::high_resolution_clock::now();
}

/* @brief Returns the elapsed time
 *
 * This function returns the elapsed time since the last startClock() call
 *
 * @return Elapsed time in microseconds
 */
long long Timer::elapsed() const { return this->m_totalElapsed; }

/* @brief Returns the elapsed time
 *
 * This function returns the elapsed time since the last startClock() call
 *
 * @return Elapsed time in microseconds
 */
long long Timer::globalElapsed() const { return this->m_totalGlobalElapsed; }

/* @brief Returns the last elapsed time
 *
 * This function returns the elapsed time since the last startClock() call
 *
 * @return Elapsed time in microseconds
 */
bool Timer::running() const { return this->m_running; }
