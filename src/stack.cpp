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
#include "stack.h"

#include <algorithm>
#include <iostream>

#include "stackimpl.h"

using namespace SmartStack;

/* @brief Returns a pointer to the stack implementation
 *
 * This function returns a pointer to the stack implementation.  This
 * function is used to ensure that the stack implementation is only
 * instantiated once. Singleton pattern.
 *
 * @return Pointer to the stack implementation
 */
SmartStack::detail::StackImpl *Stack::get() {
  static detail::StackImpl instance;
  return &instance;
}

/* @brief Starts a new session
 *
 * This function starts a new session.  If a session is already running,
 * this function will do nothing.
 *
 * @param session Name of the session
 * @param procid Process ID of the session
 * @param proc0ToScreen If true, process 0 will print to the screen
 * @param logfile Name of the logfile
 */
void Stack::startSession(const std::string &session, const int &procid,
                         const bool proc0ToScreen, const std::string &logfile) {
  Stack::get()->startSession(session, procid, proc0ToScreen, logfile);
}

/* @brief Ends the current session
 *
 * This function ends the current session.  If no session is running,
 * this function will do nothing.
 */
void Stack::endSession() { Stack::get()->endSession(); }

/* @brief Starts a new function
 *
 * This function starts a new function.  If no session is running, this
 * function will also start a session with the name "default".
 *
 * @param functionName Name of the function
 * @param showStack If true, the current stack will be printed with the message
 * "Enter"
 */
void Stack::startFunction(const std::string &functionName, bool showStack) {
  if (!Stack::get()->sessionRunning()) {
    Stack::get()->startSession("default");
  }
  Stack::get()->startFunction(functionName);
  if (showStack) {
    Stack::get()->printCurrentStack("Enter");
  }
}

/* @brief Ends the current function
 *
 * This function ends the current function.  If no session is running,
 * this function will do nothing.
 *
 * @param showStack If true, the current stack will be printed with the message
 * "Return"
 */
void Stack::endFunction(bool showStack) {
  if (Stack::get()->sessionRunning()) {
    if (showStack) {
      Stack::get()->printCurrentStack("Return");
    }
    Stack::get()->endFunction();
  }
}

/* @brief Prints the current stack
 *
 * This function prints the current stack to the screen.
 *
 * @param message Message to print after the stack
 */
void Stack::printCurrentStack(const std::string &message) {
  Stack::get()->printCurrentStack(message);
}

/* @brief Prints the current function
 *
 * This function prints the current function to the screen.
 *
 * @param message Message to print after the function
 */
void Stack::printCurrentFunction(const std::string &message) {
  Stack::get()->printCurrentFunction(message);
}

/* @brief Returns the current stack
 *
 * This function returns the current stack as a string.
 *
 * @return Current stack as a string
 */
std::string Stack::getCurrentStack() { return Stack::get()->getCurrentStack(); }

/* @brief Returns the current function
 *
 * This function returns the current function as a string.
 *
 * @return Current function as a string
 */
std::string Stack::getCurrentFunction() {
  return Stack::get()->getCurrentFunction();
}

/* @brief Returns the current session status
 *
 * This function returns the current session status as a boolean.
 *
 * @return Current session status as a boolean
 */
bool Stack::sessionStarted() { return Stack::get()->sessionRunning(); }
