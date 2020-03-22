#include "stack.h"

#include <algorithm>
#include <iostream>

using namespace SmartStack;

bool sortFunctionTimeDecending(const std::unique_ptr<Function> &a,
                               const std::unique_ptr<Function> &b) {
  return a.get()->timer()->elapsed() > b.get()->timer()->elapsed();
}

bool sortFunctionTimeAscending(const std::unique_ptr<Function> &a,
                               const std::unique_ptr<Function> &b) {
  return a.get()->timer()->elapsed() < b.get()->timer()->elapsed();
}

bool sortFunctionCallsDecending(const std::unique_ptr<Function> &a,
                                const std::unique_ptr<Function> &b) {
  return a.get()->numCalls() > b.get()->numCalls();
}

bool sortFunctionCallsAscending(const std::unique_ptr<Function> &a,
                                const std::unique_ptr<Function> &b) {
  return a.get()->numCalls() < b.get()->numCalls();
}

Stack::Stack() : m_started(false), m_firstProfile(true) {}

Stack::~Stack() { this->m_endSession(); }

void Stack::startSession(const std::string session) {
  Stack::get().m_startSession(session);
}

void Stack::endSession() { Stack::get().m_endSession(); }

void Stack::startFunction(const std::string &functionName) {
  if (!Stack::get().m_sessionStarted()) {
    Stack::get().startSession("NoSessionGiven");
  }
  Stack::get().m_startFunction(functionName);
}

void Stack::endFunction() { Stack::get().m_endFunction(); }

void Stack::printCurrentStack() { Stack::get().m_printCurrentStack(); }

void Stack::printTimingReport(SortType st, SortOrder so) {
  Stack::get().m_printTimingReport(st, so);
}

bool Stack::sessionStarted() { return Stack::get().m_sessionStarted(); }

bool Stack::m_sessionStarted() { return this->m_started; }

void Stack::m_startSession(const std::string &session) {
  this->m_sessionName = session;
  this->m_started = true;
}

void Stack::m_endSession() {
  for (auto &f : this->m_functions) {
    if (f->running()) {
      f->endFunction();
    }
  }
}

Function *Stack::getFunctionPointer(const std::string &functionName) {
  auto f = this->m_functionLookup.find(functionName);
  if (f == this->m_functionLookup.end()) {
    return this->createFunction(functionName);
  } else {
    return f->second;
  }
}

Function *Stack::createFunction(const std::string &name) {
  std::unique_ptr<Function> f(new Function(name));
  this->m_functionLookup[name] = f.get();
  this->m_functions.push_back(std::move(f));
  return this->m_functions.back().get();
}

void Stack::m_startFunction(const std::string &functionName) {
  Function *f = this->getFunctionPointer(functionName);
  f->startFunction();
  this->m_functionStack.push_back(f);
  return;
}

void Stack::m_endFunction() {
  Function *f = this->m_functionStack.back();
  f->endFunction();
  this->m_functionStack.pop_back();
  return;
}

void Stack::m_printCurrentStack() {
  bool first = true;
  for (const auto &f : this->m_functionStack) {
    if (!first) {
      std::cout << " --> " << f->name();
    } else {
      std::cout << f->name();
      first = false;
    }
  }
  std::cout << std::endl;
  std::cout.flush();
}

void Stack::sortFunctions(Stack::SortType st, Stack::SortOrder so) {
  if (st == Time && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeAscending);
  } else if (st == Time && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeDecending);
  } else if (st == Calls && so == Ascending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionCallsAscending);
  } else if (st == Calls && so == Decending) {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionCallsDecending);
  } else {
    std::sort(this->m_functions.begin(), this->m_functions.end(),
              sortFunctionTimeDecending);
  }
  return;
}

void Stack::m_printTimingReport(SortType st, SortOrder so) {
  this->sortFunctions(st, so);
  std::cout << "Function report for: " << this->m_sessionName << std::endl;
  // clang-format off
    std::cout << "|----------|--------------------------------|-------------|------------------|" << std::endl;
    std::cout << "|   Rank   |         Function Name          |    Calls    |   Duration (s)   |" << std::endl;
    std::cout << "|----------|--------------------------------|-------------|------------------|" << std::endl;
  // clang-format on

  size_t i = 0;
  for (auto &f : this->m_functions) {
    i++;
    long long t = f->timer()->elapsed() / 1e6;
    double ts = static_cast<double>(t) +
                static_cast<double>(f->timer()->elapsed() - t) / 1e6;
    printf("| %8zu | %30s | %11lld | %16.4f | \n", i, f->name().c_str(),
           f->numCalls(), ts);
  }
  fflush(stdout);
  // clang-format off
  std::cout << "|----------|--------------------------------|-------------|------------------|" << std::endl;
  // clang-format on
  return;
}
