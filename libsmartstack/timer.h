#ifndef TIMER_H
#define TIMER_H

#include <chrono>

class Timer {
 public:
  Timer();
  void startClock();
  void stopClock();
  long long elapsed() const;
  long long startTime() const;
  long long endTime() const;
  long long lastElapsed() const;
  bool running() const;

 private:
  long long m_totalElapsed;
  long long m_lastElapsed;
  bool m_running;
  std::chrono::high_resolution_clock::time_point m_start;
  std::chrono::high_resolution_clock::time_point m_end;
};

#endif  // TIMER_H
