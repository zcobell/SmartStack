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
// along long with SmartStack.  If not, see <http://www.gnu.org/licenses/>.
//------------------------------------------------------------------------//
#ifndef SMARTSTACK_FTN_H
#define SMARTSTACK_FTN_H

#include <array>
#include <cstring>
#include <iostream>
#include <memory>

#include "smartstack.h"
#include "smartstack_types.h"

// C/Fortran interface
extern "C" {
void startSessionFtn(char* sessionName, int procid, bool proc0ToScreen);
void startSessionLogFtn(char* sessionName, int procid, bool proc0ToScreen,
                        char* logfile);
void endSessionFtn();
long long addSmartStackShowFtn(char* functionName);
long long addSmartStackFtn(char* functionName);
void deleteSmartStackFtn(long long ptr);
void printCurrentStackFtn();
void printCurrentStackMessageFtn(const char* message);
void printCurrentFunctionFtn();
void printCurrentFunctionMessageFtn(const char* message);
void printTimingReportFtn(int units = 30000, int sortType = 20000,
                          int sortOrder = 10001);
void saveTimingReportFtn(char* filename, int units = 30000,
                         int sortType = 20000, int sortOrder = 10001,
                         int format = 40000);
void getCurrentStackFtn();
void getCurrentFunctionFtn();

void c2f_copyStringToFortran(const char*, int n);
}

// Hash map to store the instrumentation objects as pointers
static SmartStack::Types::HashMap<
    int, std::unique_ptr<SmartStack::Instrumentation>>::type
    s_instrumentation_mapping;
// Counter to keep track of the number of instrumentation objects
static long long s_counter = 0;

// List of possible units for the report output
constexpr std::array<SmartStack::Report::SortType, 5> c_sortTypeList = {
    SmartStack::Report::SortType::Time, SmartStack::Report::SortType::MeanTime,
    SmartStack::Report::SortType::Calls,
    SmartStack::Report::SortType::TotalTime,
    SmartStack::Report::SortType::MeanTotalTime};

// List of possible sort orders for the report output
constexpr std::array<SmartStack::Report::SortOrder, 2> c_sortOrderList = {
    SmartStack::Report::SortOrder::Ascending,
    SmartStack::Report::SortOrder::Descending};

// List of possible units for the report output
constexpr std::array<SmartStack::Report::TimeUnits, 5> c_unitsList = {
    SmartStack::Report::Microseconds, SmartStack::Report::Milliseconds,
    SmartStack::Report::Seconds, SmartStack::Report::Minutes,
    SmartStack::Report::Hours};

// List of possible output formats for the report output
constexpr std::array<SmartStack::Report::OutputFormat, 2> c_outputFormat = {
    SmartStack::Report::Table, SmartStack::Report::CSV};

/* Fortran interface functions */

/* @brief Start a session
 * @param sessionName Name of the session
 * @param procid Process ID
 * @param proc0ToScreen If true, process 0 will print to screen
 */
void startSessionFtn(char* sessionName, int procid, bool proc0ToScreen) {
  SmartStack::startSession(sessionName, procid, proc0ToScreen);
}

/* @brief Start a session with a log file
 * @param sessionName Name of the session
 * @param procid Process ID
 * @param proc0ToScreen If true, process 0 will print to screen
 * @param logfile Name of the log file
 */
void startSessionLogFtn(char* sessionName, int procid, bool proc0ToScreen,
                        char* logfile) {
  SmartStack::startSession(sessionName, procid, proc0ToScreen, logfile);
}

/* @brief End the session */
void endSessionFtn() { SmartStack::endSession(); }

/* @brief Add a new instrumentation object to the hash map
 * @param functionName Name of the function to be instrumented
 * @return Integer ID for the instrumentation object
 */
long long addSmartStackFtn(char* functionName) {
  s_counter += 1;
  s_instrumentation_mapping[s_counter] =
      std::make_unique<SmartStack::Instrumentation>(functionName);
  return s_counter;
}

/* @brief Add a new instrumentation object to the hash map and print the stack
 * @param functionName Name of the function to be instrumented
 * @return Integer ID for the instrumentation object
 */
long long addSmartStackShowFtn(char* functionName) {
  s_counter += 1;
  s_instrumentation_mapping[s_counter] =
      std::make_unique<SmartStack::Instrumentation>(functionName, true);
  return s_counter;
}

/* @brief Delete an instrumentation object from the hash map
 * @param ptr Integer ID for the instrumentation object
 */
void deleteSmartStackFtn(long long ptr) {
  s_instrumentation_mapping.erase(ptr);
}

/* @brief Print the current stack */
void printCurrentStackFtn() { SmartStack::printStack(); }

/* @brief Print the current stack with a message
 * @param message Message to be printed
 */
void printCurrentStackMessageFtn(const char* message) {
  SmartStack::printStack(message);
}

/* @brief Print the current function */
void printCurrentFunctionFtn() { SmartStack::printFunction(); }

/* @brief Print the current function with a message
 * @param message Message to be printed
 */
void printCurrentFunctionMessageFtn(const char* message) {
  SmartStack::printFunction(message);
}

/* @brief Get the current stack as a string
 *
 * The data is copied into a Fortran string using the c2f_copyStringToFortran
 */
void getCurrentStackFtn() {
  std::string s = SmartStack::getCurrentStack();
  c2f_copyStringToFortran(s.c_str(), s.size());
  return;
}

/* @brief Get the current function as a string
 *
 * The data is copied into a Fortran string using the c2f_copyStringToFortran
 */
void getCurrentFunctionFtn() {
  std::string s = SmartStack::getCurrentFunction();
  c2f_copyStringToFortran(s.c_str(), s.size());
  return;
}

/* @brief Print the timing report
 * @param units Units for the report output
 * @param sortType Type of sorting for the report output
 * @param sortOrder Order of sorting for the report output
 */
void printTimingReportFtn(int time_units, int sortType, int sortOrder) {
  const auto t_units = static_cast<size_t>(time_units) - 30000;
  const auto s_type = static_cast<size_t>(sortType) - 20000;
  const auto s_order = static_cast<size_t>(sortOrder) - 10000;
  SmartStack::Report::TimeUnits c_units = c_unitsList[t_units];
  SmartStack::Report::SortType c_sortType = c_sortTypeList[s_type];
  SmartStack::Report::SortOrder c_sortOrder = c_sortOrderList[s_order];
  SmartStack::Report::printTimingReport(c_units, c_sortType, c_sortOrder);
}

/* @brief Print the timing report
 * @param units Units for the report output
 * @param sortType Type of sorting for the report output
 * @param sortOrder Order of sorting for the report output
 * @param format Format for the report output
 */
void saveTimingReportFtn(char* filename, int time_units, int sortType,
                         int sortOrder, int format) {
  const auto t_units = static_cast<size_t>(time_units) - 30000;
  const auto s_type = static_cast<size_t>(sortType) - 20000;
  const auto s_order = static_cast<size_t>(sortOrder) - 10000;
  const auto s_format = static_cast<size_t>(format) - 40000;
  SmartStack::Report::TimeUnits c_units = c_unitsList[t_units];
  SmartStack::Report::SortType c_sortType = c_sortTypeList[s_type];
  SmartStack::Report::SortOrder c_sortOrder = c_sortOrderList[s_order];
  SmartStack::Report::OutputFormat c_format = c_outputFormat[s_format];
  SmartStack::Report::saveTimingReport(filename, c_units, c_sortType,
                                       c_sortOrder, c_format);
}
#endif  // SMARTSTACK_FTN_H