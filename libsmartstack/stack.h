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
#include "report.h"

#ifdef USE_ABSEIL_FLAT_MAP
#include "absl/container/flat_hash_map.h"
#else
#include <unordered_map>
#endif

namespace SmartStack {

class Stack {
public:
  explicit Stack(size_t reserve = 0);

  ~Stack();

  //...Returns a static instance to the SmartStack
  static Stack *instance();

  static void startSession(const std::string &session,
                           const int &processorId = -1,
                           bool proc0ToScreen = false,
                           const std::string &logfile = std::string());
  static void endSession();
  static void startFunction(const std::string &functionName,
                            bool showStack = false);
  static void endFunction(bool showStack = false);
  static void printCurrentStack(const std::string &message = std::string());
  static void printCurrentFunction(const std::string &message = std::string());
  static std::string getCurrentStack();
  static std::string getCurrentFunction();
  static bool sessionStarted();
  static std::vector<Function *> getFunctionList();
  static std::string sessionName();

private:
  int m_procid;
  bool m_started;
  bool m_firstProfile;
  bool m_logToFile;
  bool m_proc0toScreen;
  std::string m_sessionName;
  std::string m_logfile;
  std::string m_procString;
  std::vector<std::unique_ptr<Function>> m_functions;
  std::vector<Function *> m_functionStack;

#ifdef USE_ABSEIL_FLAT_MAP
  absl::flat_hash_map<std::string, Function *> m_functionLookup;
#else
  std::unordered_map<std::string, Function *> m_functionLookup;
#endif

  bool m_sessionStarted() const;
  void m_startSession(const std::string &session, int procid = -1,
                      bool proc0ToScreen = false,
                      const std::string &logfile = std::string());
  void m_endSession();
  void m_endFunction();
  void m_startFunction(const std::string &functionName);
  void m_printCurrentFunction(const std::string &message = std::string()) const;
  void m_printCurrentStack(const std::string &message = std::string()) const;
  std::string
  m_getCurrentStack(const std::string &message = std::string()) const;
  std::string
  m_getCurrentFunction(const std::string &message = std::string()) const;
  Function *createFunction(const std::string &name);
  Function *getFunctionPointer(const std::string &name);
};
} // namespace SmartStack

#endif // STACK_H
