//
// Created by Zach Cobell on 2/19/23.
//

#include "stackimpl.h"

#include <algorithm>
#include <fstream>
#include <iostream>

#define FMT_HEADER_ONLY
#include "fmt/format.h"

using namespace SmartStack::Stack::detail;

StackImpl::StackImpl(size_t reserve)
    : m_procid(-1), m_logfile(std::string()), m_procString(std::string()),
      m_started(false), m_firstProfile(true), m_logToFile(false),
      m_proc0toScreen(false) {
  if (reserve > 0) {
    this->m_functionStack.reserve(reserve);
    this->m_functions.reserve(reserve);
    this->m_functionLookup.reserve(reserve);
  }
}

StackImpl::~StackImpl() { this->endSession(); }

bool StackImpl::sessionStarted() const { return this->m_started; }

void StackImpl::startSession(const std::string &session, int procid,
                             bool proc0ToScreen, const std::string &logfile) {
  this->m_sessionName = session;
  this->m_started = true;
  this->m_procid = procid;
  this->m_proc0toScreen = proc0ToScreen;

  if (this->m_procid != -1) {
    this->m_procString = fmt::format("Processor {:6.6d}", m_procid);
  } else {
    this->m_procString = "";
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
  for (auto &f : this->m_functions) {
    if (f->running()) {
      f->endFunction();
    }
  }
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

std::string StackImpl::sessionName() const { return this->m_sessionName; }

std::vector<std::unique_ptr<Function>> &StackImpl::functions() {
  return this->m_functions;
}

std::unordered_map<std::string, Function *> &StackImpl::functionLookup() {
  return this->m_functionLookup;
}

Function *StackImpl::getFunctionPointer(const std::string &functionName) {
  auto f = this->m_functionLookup.find(functionName);
  if (f == this->m_functionLookup.end()) {
    return createFunction(functionName);
  } else {
    return f->second;
  }
}

Function *StackImpl::createFunction(const std::string &name) {
  this->m_functions.emplace_back(std::make_unique<Function>(name));
  auto ptr = this->m_functions.back().get();
  this->m_functionLookup[name] = ptr;
  return ptr;
}