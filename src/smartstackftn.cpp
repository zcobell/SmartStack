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
#include <array>
#include <cstring>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

#include "smartstack.h"

extern "C" {
void startSessionFtn(char* sessionName, int procid, bool proc0ToScreen);
void startSessionLogFtn(char* sessionName, int procid, bool proc0ToScreen,
                        char* logfile);
void endSessionFtn();
int addSmartStackShowFtn(char* functionName);
int addSmartStackFtn(char* functionName);
void deleteSmartStackFtn(int ptr);
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

static std::unordered_map<int, std::unique_ptr<SmartStack::Instrumentation>>
    s_instrumentation_mapping;
static int s_counter = 0;

constexpr std::array<SmartStack::Report::SortType, 3> c_sortTypeList = {
    SmartStack::Report::SortType::Time, SmartStack::Report::SortType::MeanTime,
    SmartStack::Report::SortType::Calls};

constexpr std::array<SmartStack::Report::SortOrder, 2> c_sortOrderList = {
    SmartStack::Report::SortOrder::Ascending,
    SmartStack::Report::SortOrder::Descending};

constexpr std::array<SmartStack::Report::TimeUnits, 5> c_unitsList = {
    SmartStack::Report::Microseconds, SmartStack::Report::Milliseconds,
    SmartStack::Report::Seconds, SmartStack::Report::Minutes,
    SmartStack::Report::Hours};

constexpr std::array<SmartStack::Report::OutputFormat, 2> c_outputFormat = {
    SmartStack::Report::Table, SmartStack::Report::CSV};

void startSessionFtn(char* sessionName, int procid, bool proc0ToScreen) {
  SmartStack::startSession(sessionName, procid, proc0ToScreen);
}

void startSessionLogFtn(char* sessionName, int procid, bool proc0ToScreen,
                        char* logfile) {
  SmartStack::startSession(sessionName, procid, proc0ToScreen, logfile);
}

void endSessionFtn() { SmartStack::endSession(); }

int addSmartStackFtn(char* functionName) {
  s_counter += 1;
  s_instrumentation_mapping[s_counter] =
      std::make_unique<SmartStack::Instrumentation>(functionName);
  return s_counter;
}

int addSmartStackShowFtn(char* functionName) {
  s_counter += 1;
  s_instrumentation_mapping[s_counter] =
      std::make_unique<SmartStack::Instrumentation>(functionName, true);
  return s_counter;
}

void deleteSmartStackFtn(int ptr) { s_instrumentation_mapping.erase(ptr); }

void printCurrentStackFtn() { SmartStack::printStack(); }

void printCurrentStackMessageFtn(const char* message) {
  SmartStack::printStack(message);
}

void printCurrentFunctionFtn() { SmartStack::printFunction(); }

void printCurrentFunctionMessageFtn(const char* message) {
  SmartStack::printFunction(message);
}

void getCurrentStackFtn() {
  std::string s = SmartStack::getCurrentStack();
  c2f_copyStringToFortran(s.c_str(), s.size());
  return;
}

void getCurrentFunctionFtn() {
  std::string s = SmartStack::getCurrentFunction();
  c2f_copyStringToFortran(s.c_str(), s.size());
  return;
}

void printTimingReportFtn(int time_units, int sortType, int sortOrder) {
  SmartStack::Report::TimeUnits c_units = c_unitsList[time_units - 30000];
  SmartStack::Report::SortType c_sortType = c_sortTypeList[sortType - 20000];
  SmartStack::Report::SortOrder c_sortOrder =
      c_sortOrderList[sortOrder - 10000];
  SmartStack::Report r;
  r.printTimingReport(c_units, c_sortType, c_sortOrder);
}

void saveTimingReportFtn(char* filename, int time_units, int sortType,
                         int sortOrder, int format) {
  SmartStack::Report::TimeUnits c_units = c_unitsList[time_units - 30000];
  SmartStack::Report::SortType c_sortType = c_sortTypeList[sortType - 20000];
  SmartStack::Report::SortOrder c_sortOrder =
      c_sortOrderList[sortOrder - 10000];
  SmartStack::Report::OutputFormat c_format = c_outputFormat[format - 40000];
  SmartStack::Report r;
  r.saveTimingReport(filename, c_units, c_sortType, c_sortOrder, c_format);
}
