#include "function.h"

Function::Function(const std::string &name) : m_name(name), m_ncall(0) {}

void Function::startFunction() {
  this->m_timer.startClock();
  this->m_ncall++;
}

void Function::endFunction() { this->m_timer.stopClock(); }

std::string Function::name() const { return m_name; }

long long Function::numCalls() const { return this->m_ncall; }

Timer *Function::timer() { return &this->m_timer; }

bool Function::running() const { return this->m_timer.running(); }
