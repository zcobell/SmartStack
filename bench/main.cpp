//------------------------------GPL---------------------------------------//
// This file is part of SmartStack.
//
// (c) 2020 Zachary Cobell
//
// SmartStack is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// SmartStack is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with SmartStack.  If not, see <http://www.gnu.org/licenses/>.
//------------------------------------------------------------------------//
#include <iostream>

#include "benchmark/benchmark.h"
#include "smartstack.h"

static void bench_lookupExistingFunction(benchmark::State& state) {
  const int nFunctions = 500;
  SmartStack::Stack::startSession("Benchmark");
  SmartStack::Stack::startFunction("myfunction");
  SmartStack::Stack::endFunction();

  std::vector<std::string> functionList(nFunctions);

  for (size_t i = 0; i < nFunctions; ++i) {
    char fn[18];
    sprintf(fn, "function_%6.6zu", i);
    functionList[i] = fn;
    SmartStack::Stack::startFunction(fn);
    SmartStack::Stack::endFunction();
  }

  while (state.KeepRunning()) {
    state.PauseTiming();
    std::string fn = functionList[rand() % nFunctions];
    state.ResumeTiming();
    SmartStack::Stack::startFunction(fn);
    SmartStack::Stack::endFunction();
  }
}

static void bench_lookupNewFunction(benchmark::State& state) {
  size_t i = 0;
  while (state.KeepRunning()) {
    state.PauseTiming();
    i++;
    char fn[21];
    sprintf(fn, "newfunction_%6.6zu", i);
    state.ResumeTiming();
    SmartStack::Stack::startFunction(fn);
    SmartStack::Stack::endFunction();
  }
  SmartStack::Stack::endSession();
}

BENCHMARK(bench_lookupExistingFunction);
BENCHMARK(bench_lookupNewFunction);

BENCHMARK_MAIN();
