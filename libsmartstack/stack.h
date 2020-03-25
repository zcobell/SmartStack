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
  enum SortType { Time, MeanTime, Calls };

  static void SMARTSTACK_EXPORT startSession(const std::string &session);
  static void SMARTSTACK_EXPORT endSession();
  static void SMARTSTACK_EXPORT startFunction(const std::string &functionName,
                                              bool showStack = false);
  static void SMARTSTACK_EXPORT endFunction(bool showStack = false);
  static void SMARTSTACK_EXPORT printCurrentStack();
  static void SMARTSTACK_EXPORT printTimingReport(
      const Stack::SortType &st = Time, const Stack::SortOrder &so = Decending);
  static void SMARTSTACK_EXPORT saveTimingReport(
      const std::string &filename, const Stack::SortType &st = Time,
      const Stack::SortOrder &so = Decending);
  static bool SMARTSTACK_EXPORT sessionStarted();

#ifndef SMARTSTACK_BENCHMARKING
 private:
#else
#warning This library is being built in benchmarking mode. This is dangerous. You have been warned.
#endif

  bool m_started;
  bool m_firstProfile;
  std::string m_sessionName;

  std::vector<std::unique_ptr<Function>> m_functions;
  std::vector<Function *> m_functionStack;

#ifdef USE_ABSEIL_FLAT_MAP
  absl::flat_hash_map<std::string, Function *> m_functionLookup;
#else
  std::unordered_map<std::string, Function *> m_functionLookup;
#endif

  bool m_sessionStarted();
  void m_startSession(const std::string &session);
  void m_endSession();
  void m_endFunction();
  void m_startFunction(const std::string &functionName);
  void m_printCurrentStack(const std::string &message = std::string());
  void m_printTimingReport(const std::vector<std::string> &report);
  void m_saveTimimgReport(const std::vector<std::string> &report,
                          const std::string &filename);
  void sortFunctions(const SortType &st, const SortOrder &so);
  void getSortCodes(std::string &calls, std::string &duration,
                    std::string &meanDuration, const Stack::SortType &st,
                    const Stack::SortOrder &so);
  std::vector<std::string> generateTimingReport(const SortType &st,
                                                const SortOrder &so);

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
