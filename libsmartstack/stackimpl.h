//
// Created by Zach Cobell on 2/19/23.
//

#ifndef SMARTSTACK_LIBSMARTSTACK_STACKIMPL_H_
#define SMARTSTACK_LIBSMARTSTACK_STACKIMPL_H_

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

namespace SmartStack::Stack::detail {
class StackImpl {

public:
  explicit StackImpl(size_t reserve = 0);

  ~StackImpl();

  bool sessionStarted() const;
  void startSession(const std::string &session, int procid = -1,
                    bool proc0ToScreen = false,
                    const std::string &logfile = std::string());
  void endSession();
  void endFunction();
  void startFunction(const std::string &functionName);

  void printCurrentFunction(const std::string &message = std::string()) const;
  void printCurrentStack(const std::string &message = std::string()) const;
  std::string sessionName() const;
  std::string getCurrentStack(const std::string &message = std::string()) const;
  std::string
  getCurrentFunction(const std::string &message = std::string()) const;
  Function *createFunction(const std::string &name);
  Function *getFunctionPointer(const std::string &name);

  std::vector<std::unique_ptr<Function>> &functions();
  std::unordered_map<std::string, Function *> &functionLookup();

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
};
} // namespace SmartStack::Stack::detail
#endif // SMARTSTACK_LIBSMARTSTACK_STACKIMPL_H_
