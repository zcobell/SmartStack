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

#include <utility>

Function::Function(std::string name) : m_name(std::move(name)), m_ncall(0) {}

void Function::startFunction() {
  this->m_timer.startClock();
  this->m_ncall++;
}

void Function::pauseFunction() { this->m_timer.pause(); }

void Function::restartFunction() { this->m_timer.restart(); }

long long Function::meanDuration() const {
  return this->m_timer.elapsed() / this->m_ncall;
}

long long Function::meanGlobalDuration() const {
  return this->m_timer.globalElapsed() / this->m_ncall;
}

void Function::endFunction() { this->m_timer.stopClock(); }

std::string Function::name() const { return this->m_name; }

long long Function::numCalls() const { return this->m_ncall; }

Timer *Function::timer() const { return &this->m_timer; }

bool Function::running() const { return this->m_timer.running(); }
