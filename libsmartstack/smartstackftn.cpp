

#include <memory>

#include "smartstack.h"
#include <iostream>

extern "C" {
void* addSmartStack(const char* functionName);
}

void* addSmartStack(const char* functionName) {
  std::cout << functionName << std::endl;
  SmartStack::Instrumentation* s =
      new SmartStack::Instrumentation(functionName);
  return (void*)s;
}
