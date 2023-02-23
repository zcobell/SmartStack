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
#ifndef SMARTSTACK_H
#define SMARTSTACK_H

#include <string>

#include "instrumentation.h"

/* This is the main header file for SmartStack.  It contains the
 * functions that are used to start and end a session, as well as
 * the macros that are used to add instrumentation to a function.
 */

/* This macro is used to add instrumentation to a function.  It
 * should be placed at the beginning of the function.  The function
 * name is passed as a string.  The macro will create a static
 * variable that will be used to track the function.
 */
#define ADD_SMARTSTACK(fname)                          \
  SmartStack::Instrumentation __SmartStackInstrument = \
      SmartStack::addInstrumentation(fname);

/* This macro is used to add instrumentation to a function.  It
 * should be placed at the beginning of the function.  The function
 * name is passed as a string.  The macro will create a static
 * variable that will be used to track the function.  This
 * version of the macro will also print the entry/return of the
 * function to the screen.
 */
#define ADD_SMARTSTACK_LOGGING(fname)                  \
  SmartStack::Instrumentation __SmartStackInstrument = \
      SmartStack::addInstrumentation(fname, true);

/* This macro is used to add instrumentation to a function.  It
 * should be placed at the beginning of the function.  The function
 * name is automatically determined. The macro will create a static
 * variable that will be used to track the function based on lifetime.
 */
#ifdef __func__
#define AUTOADD_SMARTSTACK() ADD_SMARTSTACK(__func__)
#else
#define AUTOADD_SMARTSTACK #error "__func__ not defined"
#endif

namespace SmartStack {

/* This function returns the current session status.  It returns
 * true if a session has been started, false otherwise.
 */
bool sessionStarted() { return SmartStack::Stack::sessionStarted(); }

/* This function starts a session in the SmartStack library.  It
 * should be called at the beginning of the program.  The session
 * name is passed as a string.  The procid is the MPI process ID
 * (or -1 if not using MPI).  The proc0ToScreen flag is used to
 * determine if the output from process 0 should be sent to the
 * screen.  The logfile is the name of the file to which the output
 * should be written.
 */
void startSession(const std::string &sessionName, const int &procid = -1,
                  const bool proc0ToScreen = false,
                  const std::string &logfile = std::string()) {
  SmartStack::Stack::startSession(sessionName, procid, proc0ToScreen, logfile);
}

/* This function ends a session in the SmartStack library.  It
 * should be called at the end of the program.
 */
void endSession() { SmartStack::Stack::endSession(); }

/* This function prints the current stack to the screen.  The
 * message is an optional string that will be printed after the
 * stack.
 */
void printStack(const std::string &message = std::string()) {
  SmartStack::Stack::printCurrentStack(message);
}

/* This function prints the current function to the screen.  The
 * message is an optional string that will be printed after the
 * function.
 */
void printFunction(const std::string &message = std::string()) {
  SmartStack::Stack::printCurrentFunction(message);
}

/* This function prints the timing report to the screen.  The
 * timeUnits is an optional parameter that can be used to change
 * the units of the timing report.  The sortType is an optional
 * parameter that can be used to change the sort type of the
 * timing report.  The sortOrder is an optional parameter that
 * can be used to change the sort order of the timing report.
 * The default is to sort by time in descending order.
 *
 * @param timeUnits The units of the timing report
 * @param sortType The sort type of the timing report
 * @param sortOrder The sort order of the timing report
 *
 */
void printTimingReport(
    SmartStack::Report::TimeUnits timeUnits = SmartStack::Report::Milliseconds,
    SmartStack::Report::SortType sortType = SmartStack::Report::SortType::Time,
    SmartStack::Report::SortOrder sortOrder =
        SmartStack::Report::SortOrder::Descending) {
  SmartStack::Report::printTimingReport(timeUnits, sortType, sortOrder);
}

/* This function saves the timing report to a file.  The filename
 * is the name of the file to which the timing report should be
 * saved.  The format is an optional parameter that can be used
 * to change the format of the timing report.  The timeUnits is
 * an optional parameter that can be used to change the units of
 * the timing report.  The sortType is an optional parameter that
 * can be used to change the sort type of the timing report.  The
 * sortOrder is an optional parameter that can be used to change
 * the sort order of the timing report.  The default is to sort
 * by time in descending order.
 *
 * @param filename The name of the file to which the timing report
 *                 should be saved
 * @param format The format of the timing report
 * @param timeUnits The units of the timing report
 * @param sortType The sort type of the timing report
 * @param sortOrder The sort order of the timing report
 *
 */
void saveTimingReport(
    const std::string &filename,
    SmartStack::Report::OutputFormat format = SmartStack::Report::Table,
    SmartStack::Report::TimeUnits timeUnits = SmartStack::Report::Milliseconds,
    SmartStack::Report::SortType sortType = SmartStack::Report::SortType::Time,
    SmartStack::Report::SortOrder sortOrder =
        SmartStack::Report::SortOrder::Descending) {
  SmartStack::Report::saveTimingReport(filename, timeUnits, sortType, sortOrder,
                                       format);
}

/* This function prints the call graph to the screen.  The
 * timeUnits is an optional parameter that can be used to change
 * the units of the call graph.  The sortType is an optional
 * parameter that can be used to change the sort type of the
 * call graph.  The sortOrder is an optional parameter that can
 * be used to change the sort order of the call graph.  The
 * default is to sort by time in descending order.
 *
 * @param timeUnits The units of the call graph
 * @param sortType The sort type of the call graph
 * @param sortOrder The sort order of the call graph
 *
 */
std::string getCurrentStack() { return SmartStack::Stack::getCurrentStack(); }

/* This function prints the current function to the screen.  The
 * message is an optional string that will be printed after the
 * function.
 */
std::string getCurrentFunction() {
  return SmartStack::Stack::getCurrentFunction();
}

/* This function adds instrumentation to a function.  The
 * functionName is the name of the function.  The showStack
 * parameter is used to determine if the entry/return of the
 * function should be printed to the screen.
 */
Instrumentation addInstrumentation(const std::string &functionName,
                                   bool showStack = false) {
  return SmartStack::Instrumentation(functionName, showStack);
}

}  // namespace SmartStack

#endif  // SMARTSTACK_H
