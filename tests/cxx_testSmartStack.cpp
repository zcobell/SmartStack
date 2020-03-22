
#include <unistd.h>

#include "smartstack.h"

void doSomething();
void doSomethingElse();

int main(int argv, char** argc) {
  SmartStack::startSession("ADCIRC");

  ADD_SMARTSTACK("main");

  for (size_t i = 0; i < 1000; ++i) {
    doSomething();
  }

  END_SMARTSTACK();
  SmartStack::printTimingReport();

  return 0;
}

void doSomething() {
  ADD_SMARTSTACK("doSomething")

  usleep(500);
  doSomethingElse();
  doSomethingElse();

  return;
}

void doSomethingElse() {
  ADD_SMARTSTACK("doSomethingElse");
  usleep(50);
}
