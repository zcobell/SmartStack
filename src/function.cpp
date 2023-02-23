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
#include "function.h"

#include <iostream>
#include <utility>

using namespace SmartStack::detail;

/**
 * @brief Function::Function
 * @param name
 */
Function::Function(std::string name) : m_name(std::move(name)), m_ncall(0) {}

/**
 * @brief Function::startFunction
 *
 * This function starts the timer for the function
 */
void Function::startFunction() {
  this->m_timer.startClock();
  this->m_ncall++;
}

/**
 * @brief Function::pauseFunction
 *
 * This function pauses the timer for the function
 */
void Function::pauseFunction() { this->m_timer.pause(); }

/**
 * @brief Function::restartFunction
 *
 * This function restarts the timer for the function
 */
void Function::restartFunction() { this->m_timer.restart(); }

/**
 * @brief Function::meanDuration
 *
 * This function returns the mean duration of the function
 */
long long Function::meanDuration() {
  return this->m_timer.elapsed() / this->m_ncall;
}

/**
 * @brief Function::meanGlobalDuration
 * @return Mean global duration of the function
 *
 * This function returns the mean global duration of the function
 */
long long Function::meanGlobalDuration() {
  return this->m_timer.globalElapsed() / this->m_ncall;
}

/**
 * @brief Function::endFunction
 *
 * This function ends the timer for the function
 */
void Function::endFunction() { this->m_timer.stopClock(); }

/**
 * @brief Function::name
 * @return Name of the function
 *
 * This function returns the name of the function
 */
std::string Function::name() const { return this->m_name; }

/**
 * @brief Function::numCalls
 * @return Number of calls to the function
 *
 * This function returns the number of calls to the function
 */
long long Function::numCalls() const { return this->m_ncall; }

/**
 * @brief Function::timer
 * @return Pointer to the timer object
 *
 * This function returns a pointer to the timer object
 */
Timer *Function::timer() { return &this->m_timer; }

/**
 * @brief Function::running
 * @return True if the function is running
 *
 * This function returns true if the function is running
 */
bool Function::running() const { return this->m_timer.running(); }
