//
// Created by zcobell on 2/20/23.
//

#include "stackimpl.h"

#include <fstream>
#include <iostream>

#include "fmt/core.h"

using namespace SmartStack::detail;

/* @brief Constructor
 *
 * This constructor creates a new stack implementation.  The constructor
 * will reserve memory for the stack if the reserve parameter is greater
 * than zero.
 *
 * @param reserve Number of elements to reserve in the stack
 */
StackImpl::StackImpl(size_t reserve)
    : m_procid(-1),
      m_sessionRunning(false),
      m_logToFile(false),
      m_proc0toScreen(false),
      m_logfile(std::string()),
      m_procString(std::string()) {
  if (reserve > 0) {
    this->m_functionStack.reserve(5000);
    this->m_functions.reserve(5000);
    this->m_functionLookup.reserve(5000);
  }
}

/* @brief Return the current session status
 *
 * This function returns the current session status.  If a session is
 * running, the function returns true.  If no session is running, the
 * function returns false.
 *
 * @return True if a session is running, false otherwise
 */
bool StackImpl::sessionRunning() const { return this->m_sessionRunning; }

/* @brief Starts a new session
 *
 * This function starts a new session.  If a session is already running,
 * this function will do nothing.
 *
 * @param session Name of the session
 * @param procid Process ID of the session
 * @param proc0ToScreen If true, process 0 will print to the screen
 * @param logfile Name of the logfile
 */
void StackImpl::startSession(const std::string &session, int procid,
                             const bool proc0ToScreen,
                             const std::string &logfile) {
  this->m_sessionName = session;
  this->m_sessionRunning = true;
  this->m_procid = procid;
  this->m_proc0toScreen = proc0ToScreen;

  if (this->m_procid != -1) {
    this->m_procString = fmt::format("Processor {:06d}", this->m_procid);
  } else {
    this->m_procString = std::string();
  }
  if (!logfile.empty()) {
    this->m_logToFile = true;
    this->m_logfile = logfile;
    std::ifstream f(this->m_logfile);
    if (f.good()) {
      remove(this->m_logfile.c_str());
    }
  } else {
    this->m_logToFile = false;
    this->m_logfile = std::string();
  }
}

/* @brief Ends the current session
 *
 * This function ends the current session.  If no session is running,
 * this function will do nothing.
 */
void StackImpl::endSession() {
  for (auto it = this->m_functionStack.rbegin();
       it != this->m_functionStack.rend(); ++it) {
    (*it)->endFunction();
  }
  this->m_sessionRunning = false;
}

/* @brief Starts a function
 *
 * This function starts a function. If the function has been started before,
 * the function will be restarted.  If the function has not been started
 * before, the function will be created and started.
 *
 * @param functionName Name of the function
 */
void StackImpl::startFunction(const std::string &functionName) {
  if (!this->m_functionStack.empty()) {
    this->m_functionStack.back()->pauseFunction();
  }
  Function *f = this->getFunctionPointer(functionName);
  f->startFunction();
  this->m_functionStack.push_back(f);
}

/* @brief Ends the current function
 *
 * This function ends the current function, removing it from the stack, and
 * restarts the previous function.
 */
void StackImpl::endFunction() {
  Function *f = this->m_functionStack.back();
  f->endFunction();
  this->m_functionStack.pop_back();
  if (!this->m_functionStack.empty()) {
    this->m_functionStack.back()->restartFunction();
  }
}

/* @brief Prints the current stack
 *
 * This function prints the current stack to the screen and/or logfile.
 * If the message parameter is not empty, the message will be printed
 * before the stack.
 *
 * @param message Message to print before the stack
 */
void StackImpl::printCurrentStack(const std::string &message) const {
  if (this->m_logToFile) {
    std::ofstream f;
    f.open(this->m_logfile, std::ios::out | std::ios::app);
    f << this->getCurrentStack(message) << std::endl;
    f.flush();
    f.close();
  }

  if (this->m_procid < 0 || (this->m_procid == 0 && this->m_proc0toScreen)) {
    std::cout << this->getCurrentStack(message) << std::endl;
  }
}

/* @brief Prints the current function
 *
 * This function prints the current function to the screen and/or logfile.
 * If the message parameter is not empty, the message will be printed
 * before the function.
 *
 * @param message Message to print before the function
 */
void StackImpl::printCurrentFunction(const std::string &message) const {
  if (this->m_logToFile) {
    std::ofstream f;
    f.open(this->m_logfile, std::ios::out | std::ios::app);
    f << this->getCurrentFunction(message) << std::endl;
    f.flush();
    f.close();
  }

  if (this->m_procid < 0 || (this->m_procid == 0 && this->m_proc0toScreen)) {
    std::cout << this->getCurrentFunction(message) << std::endl;
  }
}

/* @brief Returns the current stack
 *
 * This function returns the current stack as a string.  If the message
 * parameter is not empty, the message will be printed before the stack.
 *
 * @param message Message to print after the stack
 * @return Current stack as a string
 */
std::string StackImpl::getCurrentStack(const std::string &message) const {
  bool first = true;
  std::string s;
  if (this->m_procid == -1) {
    s = "[" + this->m_sessionName + "]: ";
  } else {
    s = "[" + this->m_sessionName + " " + this->m_procString + "]: ";
  }
  for (const auto &f : this->m_functionStack) {
    if (!first) {
      s += " --> " + f->name();
    } else {
      s += f->name();
      first = false;
    }
  }
  if (!message.empty()) {
    s += ": " + message;
  }
  return s;
}

/* @brief Returns the current function
 *
 * This function returns the current function as a string.  If the message
 * parameter is not empty, the message will be printed after the function.
 *
 * @param message Message to print after the function
 * @return Current function as a string
 */
std::string StackImpl::getCurrentFunction(const std::string &message) const {
  std::string s;
  if (this->m_procid == -1) {
    s = "[" + this->m_sessionName +
        "]: " + this->m_functionStack.back()->name();
  } else {
    s = "[" + this->m_sessionName + " " + this->m_procString +
        "]: " + this->m_functionStack.back()->name();
  }
  if (!message.empty()) {
    s += ": " + message;
  }
  return s;
}

/* @brief Returns a vector of all functions
 *
 * This function returns a vector of all functions that have been started
 * during the current session.
 *
 * @return Vector of all functions
 */
std::vector<Function *> StackImpl::functions() {
  std::vector<Function *> result;
  result.reserve(m_functions.size());
  for (auto &f : m_functions) {
    result.push_back(f.get());
  }
  return result;
}

/* @brief Returns the current session name
 *
 * This function returns the current session name.
 *
 * @return Current session name
 */
std::string StackImpl::sessionName() const { return m_sessionName; }

/* @brief Returns a pointer to the function with the given name
 *
 * This function returns a pointer to the function with the given name.
 * If the function has not been started before, it will be created.
 *
 * @param functionName Name of the function
 * @return Pointer to the function
 */
Function *StackImpl::getFunctionPointer(const std::string &functionName) {
  auto f = this->m_functionLookup.find(functionName);
  if (f == this->m_functionLookup.end()) {
    this->m_functions.emplace_back(std::make_unique<Function>(functionName));
    this->m_functionLookup[functionName] = m_functions.back().get();
    return m_functions.back().get();
  } else {
    return f->second;
  }
}

/* @brief Creates a new function
 *
 * This function creates a new function with the given name and adds it to the
 * vector of all functions as well as the hashtable.
 *
 * @param name Name of the function
 * @return Pointer to the new function
 */
Function *StackImpl::createFunction(const std::string &name) {
  this->m_functions.emplace_back(std::make_unique<Function>(name));
  this->m_functionLookup[name] = this->m_functions.back().get();
  return this->m_functions.back().get();
}
