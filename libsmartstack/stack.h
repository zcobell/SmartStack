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
#ifndef STACK_H
#define STACK_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "function.h"
#include "smartstack_global.h"

#ifdef USE_ABSEIL_FLAT_MAP
#include "absl/container/flat_hash_map.h"
#else
#include <unordered_map>
#endif

namespace SmartStack {

class Stack {
 public:
  enum SortOrder { Ascending, Decending };
  enum SortType { Time, MeanTime, TotalTime, MeanTotalTime, Calls };
  enum TimeUnits { Microseconds, Milliseconds, Seconds, Minutes, Hours };
  enum OutputFormat { Table, CSV };

  static void SMARTSTACK_EXPORT
  startSession(const std::string &session, const int &processorId = -1,
               const bool proc0ToScreen = false,
               const std::string &logfile = std::string());
  static void SMARTSTACK_EXPORT endSession();
  static void SMARTSTACK_EXPORT startFunction(const std::string &functionName,
                                              bool showStack = false);
  static void SMARTSTACK_EXPORT endFunction(bool showStack = false);
  static void SMARTSTACK_EXPORT
  printCurrentStack(const std::string &message = std::string());
  static void SMARTSTACK_EXPORT
  printCurrentFunction(const std::string &message = std::string());
  static std::string SMARTSTACK_EXPORT getCurrentStack();
  static std::string SMARTSTACK_EXPORT getCurrentFunction();
  static void SMARTSTACK_EXPORT printTimingReport(
      const Stack::SortType &st = Time, const Stack::SortOrder &so = Decending);
  static void SMARTSTACK_EXPORT saveTimingReport(
      const std::string &filename, const Stack::SortType &st = Time,
      const Stack::SortOrder &so = Decending,
      const Stack::OutputFormat &of = Stack::OutputFormat::Table);
  static bool SMARTSTACK_EXPORT sessionStarted();
  static void SMARTSTACK_EXPORT setReportUnits(const Stack::TimeUnits &units);

#ifndef SMARTSTACK_BENCHMARKING
 private:
#else
#warning This library is being built in benchmarking mode. This is dangerous. You have been warned.
#endif

  int m_procid;
  std::string m_logfile;
  std::string m_procString;
  bool m_started;
  bool m_firstProfile;
  bool m_logToFile;
  bool m_proc0toScreen;
  std::string m_sessionName;
  TimeUnits m_reportUnits;

  std::vector<std::unique_ptr<Function>> m_functions;
  std::vector<Function *> m_functionStack;

#ifdef USE_ABSEIL_FLAT_MAP
  absl::flat_hash_map<std::string, Function *> m_functionLookup;
#else
  std::unordered_map<std::string, Function *> m_functionLookup;
#endif

  bool m_sessionStarted();
  void m_startSession(const std::string &session, int procid = -1,
                      const bool proc0ToScreen = false,
                      const std::string &logfile = std::string());
  void m_endSession();
  void m_endFunction();
  void m_startFunction(const std::string &functionName);
  void m_printCurrentFunction(const std::string &message = std::string()) const;
  void m_printCurrentStack(const std::string &message = std::string()) const;
  void m_printTimingReport(const std::vector<std::string> &report) const;
  void m_saveTableTimimgReport(const std::vector<std::string> &report,
                               const std::string &filename) const;
  void m_saveCsvTimingReport(const std::string &filename);
  std::string m_getFunctionReportLine(size_t i, Function *f,
                                      const TimeUnits &units, const OutputFormat &format) const;
  std::string m_getCurrentStack(
      const std::string &message = std::string()) const;
  std::string m_getCurrentFunction(
      const std::string &message = std::string()) const;
  void sortFunctions(const SortType &st, const SortOrder &so);
  void getSortCodes(std::string &calls, std::string &duration,
                    std::string &meanDuration, std::string &totalDuration,
                    std::string &meanTotalDuration, const SortType &st,
                    const Stack::SortOrder &so);
  std::vector<std::string> generateTableTimingReport(const SortType &st,
                                                     const SortOrder &so);
  void m_setReportUnits(const Stack::TimeUnits &units);
  std::string m_unitsString(const Stack::TimeUnits &units, bool trim) const;
  double convertTimeUnitsDouble(const long long time,
                                const double multiplier) const;
  size_t maxNumFunctionChars(size_t lowerLimit = 0) const;
  std::string formatStringChar(size_t n) const;

  void writeHeader();
  void writeFooter();
  Function *createFunction(const std::string &name);
  Function *getFunctionPointer(const std::string &name);

  Stack();

  ~Stack();

  //...Returns a static instance to the SmartStack
  static Stack &get() {
    static Stack instance;
    return instance;
  }
};
}  // namespace SmartStack

#endif  // STACK_H
