#include "timer.h"

Timer::Timer() : m_totalElapsed(0), m_lastElapsed(0), m_running(false) {}

void Timer::startClock() {
  this->m_running = true;
  this->m_start = std::chrono::high_resolution_clock::now();
}

void Timer::stopClock() {
  if (this->m_running) {
    this->m_end = std::chrono::high_resolution_clock::now();
    this->m_lastElapsed = std::chrono::duration_cast<std::chrono::microseconds>(
                              this->m_end - this->m_start)
                              .count();
    this->m_totalElapsed += this->m_lastElapsed;
    this->m_running = false;
  }
}

long long Timer::elapsed() const { return this->m_totalElapsed; }

long long Timer::startTime() const {
  return std::chrono::time_point_cast<std::chrono::milliseconds>(this->m_start)
      .time_since_epoch()
      .count();
}

long long Timer::endTime() const {
  return std::chrono::time_point_cast<std::chrono::milliseconds>(this->m_end)
      .time_since_epoch()
      .count();
}

bool Timer::running() const { return this->m_running; }
