//
// Created by zcobell on 2/20/23.
//

#include "stackimpl.h"

#include <fstream>
#include <iostream>

#include "fmt/core.h"

using namespace SmartStack::detail;

StackImpl::StackImpl(size_t reserve)
    : m_procid(-1),
      m_logfile(std::string()),
      m_procString(std::string()),
      m_sessionRunning(false),
      m_logToFile(false),
      m_proc0toScreen(false) {
  if (reserve > 0) {
    this->m_functionStack.reserve(5000);
    this->m_functions.reserve(5000);
    this->m_functionLookup.reserve(5000);
  }
}

bool StackImpl::sessionRunning() const { return this->m_sessionRunning; }

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

void StackImpl::endSession() {
  for (auto it = this->m_functionStack.rbegin();
       it != this->m_functionStack.rend(); ++it) {
    (*it)->endFunction();
  }
  this->m_sessionRunning = false;
}

void StackImpl::startFunction(const std::string &functionName) {
  if (!this->m_functionStack.empty()) {
    this->m_functionStack.back()->pauseFunction();
  }
  Function *f = this->getFunctionPointer(functionName);
  f->startFunction();
  this->m_functionStack.push_back(f);
}

void StackImpl::endFunction() {
  Function *f = this->m_functionStack.back();
  f->endFunction();
  this->m_functionStack.pop_back();
  if (!this->m_functionStack.empty()) {
    this->m_functionStack.back()->restartFunction();
  }
}

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

std::vector<Function *> StackImpl::functions() {
  std::vector<Function *> result;
  result.reserve(m_functions.size());
  for (auto &f : m_functions) {
    result.push_back(f.get());
  }
  return result;
}

std::string StackImpl::sessionName() const { return m_sessionName; }

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

Function *StackImpl::createFunction(const std::string &name) {
  this->m_functions.emplace_back(std::make_unique<Function>(name));
  this->m_functionLookup[name] = this->m_functions.back().get();
  return this->m_functions.back().get();
}
