#ifndef INSTRUMENTATION_H
#define INSTRUMENTATION_H

#include <string>

#include "stack.h"

namespace SmartStack {
class Instrumentation {
 public:
  Instrumentation(const std::string &functionName, bool showStack = false)
      : m_showStack(showStack) {
    SmartStack::Stack::startFunction(functionName, this->m_showStack);
  }
  ~Instrumentation() { SmartStack::Stack::endFunction(this->m_showStack); }

 private:
  bool m_showStack;
};
}  // namespace SmartStack

#endif  // INSTRUMENTATION_H
