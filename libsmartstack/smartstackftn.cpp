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

#include "smartstack.h"

extern "C" {
void startSessionFtn(char *sessionName, int procid, bool proc0ToScreen);
void startSessionLogFtn(char *sessionName, int procid, bool proc0ToScreen,
                        char *logfile);
void endSessionFtn();
void *addSmartStackShowFtn(char *functionName);
void *addSmartStackFtn(char *functionName);
void deleteSmartStackFtn(void *ptr);
void printCurrentStackFtn();
void printCurrentStackMessageFtn(const char *message);
void printCurrentFunctionFtn();
void printCurrentFunctionMessageFtn(const char *message);
void printTimingReportFtn(int reportUnits = 30001, int sortType = 20000,
                          int sortOrder = 10001);
void saveTimingReportFtn(char *filename, int reportUnits = 30001,
                         int sortType = 20000, int sortOrder = 10001,
                         int format = 40000);
}

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

void startSessionFtn(char *sessionName, int procid, bool proc0ToScreen) {
  SmartStack::startSession(sessionName, procid, proc0ToScreen);
}

void startSessionLogFtn(char *sessionName, int procid, bool proc0ToScreen,
                        char *logfile) {
  SmartStack::startSession(sessionName, procid, proc0ToScreen, logfile);
}

void endSessionFtn() { SmartStack::endSession(); }

void *addSmartStackFtn(char *functionName) {
  SmartStack::Instrumentation *s =
      new SmartStack::Instrumentation(functionName);
  return (void *)s;
}

void *addSmartStackShowFtn(char *functionName) {
  SmartStack::Instrumentation *s =
      new SmartStack::Instrumentation(functionName, true);
  return (void *)s;
}

void deleteSmartStackFtn(void *ptr) {
  SmartStack::Instrumentation *s =
      reinterpret_cast<SmartStack::Instrumentation *>(ptr);
  delete s;
}

void printCurrentStackFtn() { SmartStack::printStack(); }

void printCurrentStackMessageFtn(const char *message) {
  SmartStack::printStack(message);
}

void printCurrentFunctionFtn() { SmartStack::printFunction(); }

void printCurrentFunctionMessageFtn(const char *message) {
  SmartStack::printFunction(message);
}

void printTimingReportFtn(int reportUnits, int sortType, int sortOrder) {
  SmartStack::Report::SortType c_sortType = c_sortTypeList[sortType - 20000];
  SmartStack::Report::SortOrder c_sortOrder =
      c_sortOrderList[sortOrder - 10000];
  SmartStack::Report::TimeUnits c_units = c_unitsList[reportUnits - 30000];
  SmartStack::printTimingReport(c_units, c_sortType, c_sortOrder);
}

void saveTimingReportFtn(char *filename, int reportUnits, int sortType,
                         int sortOrder, int format) {
  SmartStack::Report::SortType c_sortType = c_sortTypeList[sortType - 20000];
  SmartStack::Report::SortOrder c_sortOrder =
      c_sortOrderList[sortOrder - 10000];
  SmartStack::Report::OutputFormat c_format = c_outputFormat[format - 40000];
  SmartStack::Report::TimeUnits c_units = c_unitsList[reportUnits - 30000];
  SmartStack::saveTimingReport(filename, c_units, c_sortType, c_sortOrder,
                               c_format);
}
