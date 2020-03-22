#ifndef INSTRUMENTATION_H
#define INSTRUMENTATION_H

#include <string>

#include "stack.h"

namespace SmartStack {
class Instrumentation {
 public:
  Instrumentation(const std::string &functionName) {
    SmartStack::Stack::startFunction(functionName);
  }
  ~Instrumentation() { SmartStack::Stack::endFunction(); }
};
}  // namespace SmartStack

#endif  // INSTRUMENTATION_H
