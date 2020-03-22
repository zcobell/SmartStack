#ifndef STACK_H
#define STACK_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "function.h"

namespace SmartStack {

class Stack {
 public:
  enum SortOrder { Ascending, Decending };
  enum SortType { Time, MeanTime, Calls };

  static void startSession(const std::string &session);
  static void endSession();
  static void startFunction(const std::string &functionName,
                            bool showStack = false);
  static void endFunction(bool showStack = false);
  static void printCurrentStack();
  static void printTimingReport(const Stack::SortType &st = Time,
                                const Stack::SortOrder &so = Decending);
  static bool sessionStarted();

 private:
  bool m_started;
  bool m_firstProfile;
  std::string m_sessionName;

  std::vector<std::unique_ptr<Function>> m_functions;
  std::vector<Function *> m_functionStack;
  std::unordered_map<std::string, Function *> m_functionLookup;

  bool m_sessionStarted();
  void m_startSession(const std::string &session);
  void m_endSession();
  void m_endFunction();
  void m_startFunction(const std::string &functionName);
  void m_printCurrentStack(const std::string &message = std::string());
  void m_printTimingReport(const SortType &st, const SortOrder &so);
  void sortFunctions(const SortType &st, const SortOrder &so);
  void getSortCodes(std::string &calls, std::string &duration,
                    std::string &meanDuration, const Stack::SortType &st,
                    const Stack::SortOrder &so);

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
