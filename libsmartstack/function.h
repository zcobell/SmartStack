#ifndef FUNCTION_H
#define FUNCTION_H

#include <string>

#include "timer.h"

class Function {
 public:
  Function(const std::string &name);

  void startFunction();
  void endFunction();

  Timer *timer();

  std::string name() const;

  long long numCalls() const;

  bool running() const;

 private:
  const std::string m_name;
  long long m_ncall;
  Timer m_timer;
};

#endif  // FUNCTION_H
