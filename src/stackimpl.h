//
// Created by zcobell on 2/20/23.
//

#ifndef SMARTSTACK_STACKIMPL_H
#define SMARTSTACK_STACKIMPL_H

#include <memory>
#include <string>
#include <vector>

#include "function.h"
#include "report.h"
#include "smartstack_types.h"

namespace SmartStack::detail {

/* @brief Stack implementation
 *
 * This class implements the stack.
 */
class StackImpl {
 public:
  explicit StackImpl(size_t reserve = 0);

  bool sessionRunning() const;

  void startSession(const std::string &session, int procid = -1,
                    bool proc0ToScreen = false,
                    const std::string &logfile = std::string());

  void endSession();

  void endFunction();

  std::string sessionName() const;

  void startFunction(const std::string &functionName);

  void printCurrentFunction(const std::string &message = std::string()) const;

  void printCurrentStack(const std::string &message = std::string()) const;

  std::string getCurrentStack(const std::string &message = std::string()) const;

  std::string getCurrentFunction(
      const std::string &message = std::string()) const;

  Function *createFunction(const std::string &name);

  Function *getFunctionPointer(const std::string &name);

  std::vector<Function *> functions();

 private:
  int m_procid;
  bool m_sessionRunning;
  bool m_logToFile;
  bool m_proc0toScreen;
  std::string m_logfile;
  std::string m_procString;
  std::string m_sessionName;

  std::vector<std::unique_ptr<Function>> m_functions;
  std::vector<Function *> m_functionStack;
  SmartStack::Types::HashMap<std::string, SmartStack::detail::Function *>::type
      m_functionLookup;
};

}  // namespace SmartStack::detail
#endif  // SMARTSTACK_STACKIMPL_H
