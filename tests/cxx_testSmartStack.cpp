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
#include <unistd.h>

#include <iostream>

#include "smartstack.h"

void doSomething();
void doSomethingElse();

int main(int argv, char** argc) {
  SmartStack::startSession("ADCIRC");

  ADD_SMARTSTACK_LOGGING("main");

  for (size_t i = 0; i < 3; ++i) {
    doSomething();
  }

  SmartStack::endSession();
  SmartStack::printTimingReport(SmartStack::Report::Milliseconds,
                                SmartStack::Report::TotalTime,
                                SmartStack::Report::Ascending);

  std::cout << "\n";

  SmartStack::printTimingReport(SmartStack::Report::Microseconds,
                                SmartStack::Report::TotalTime,
                                SmartStack::Report::Ascending);

  return 0;
}

void doSomething() {
  ADD_SMARTSTACK_LOGGING("doSomething")

  usleep(500);
  doSomethingElse();
  doSomethingElse();

  return;
}

void doSomethingElse() {
  ADD_SMARTSTACK_LOGGING("doSomethingElse");
  usleep(50);
}
