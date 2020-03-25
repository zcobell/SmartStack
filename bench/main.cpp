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
#include <vector>

#define SMARTSTACK_BENCHMARKING

#include "benchmark/benchmark.h"
#include "smartstack.h"

const size_t c_functionSize = 500;

static void bench_vectorRandomLookup(benchmark::State& state) {
  std::vector<std::string> functionList(c_functionSize);

  for (size_t i = 0; i < c_functionSize; ++i) {
    char fn[20];
    sprintf(fn, "function_%8.8zu", i);
    functionList[i] = fn;
  }

  while (state.KeepRunning()) {
    benchmark::DoNotOptimize(functionList[rand() % c_functionSize]);
  }
}

static void bench_getFunctionPointer(benchmark::State& state) {
  std::vector<std::string> functionList(c_functionSize);
  SmartStack::Stack::startSession("Benchmark");
  SmartStack::Stack::startFunction("myfunction");
  SmartStack::Stack::endFunction();

  for (size_t i = 0; i < c_functionSize; ++i) {
    char fn[20];
    sprintf(fn, "function_%8.8zu", i);
    functionList[i] = fn;
    SmartStack::Stack::startFunction(fn);
    SmartStack::Stack::endFunction();
  }

  while (state.KeepRunning()) {
    benchmark::DoNotOptimize(SmartStack::Stack::get().getFunctionPointer(
        functionList[rand() % c_functionSize]));
  }
}

static void bench_getPointerAndStart(benchmark::State& state) {
  std::vector<std::string> functionList(c_functionSize);
  SmartStack::Stack::startSession("Benchmark");
  SmartStack::Stack::startFunction("myfunction");
  SmartStack::Stack::endFunction();

  for (size_t i = 0; i < c_functionSize; ++i) {
    char fn[20];
    sprintf(fn, "function_%8.8zu", i);
    functionList[i] = fn;
  }

  while (state.KeepRunning()) {
    Function* f = SmartStack::Stack::get().getFunctionPointer(
        functionList[rand() % c_functionSize]);
    f->startFunction();
  }
}

static void bench_lookupExistingFunction(benchmark::State& state) {
  std::vector<std::string> functionList(c_functionSize);
  SmartStack::Stack::startSession("Benchmark");
  SmartStack::Stack::startFunction("myfunction");
  SmartStack::Stack::endFunction();

  for (size_t i = 0; i < c_functionSize; ++i) {
    char fn[20];
    sprintf(fn, "function_%8.8zu", i);
    functionList[i] = fn;
  }

  while (state.KeepRunning()) {
    SmartStack::Stack::startFunction(functionList[rand() % c_functionSize]);
    SmartStack::Stack::endFunction();
  }

  SmartStack::saveTimingReport("report.txt");
}

static void bench_createFunctionString(benchmark::State& state) {
  size_t i = 0;
  while (state.KeepRunning()) {
    i++;
    char fn[21];
    benchmark::DoNotOptimize(sprintf(fn, "newfunction_%8.8zu", i));
  }
}

static void bench_lookupNewFunction(benchmark::State& state) {
  size_t i = 0;
  while (state.KeepRunning()) {
    i++;
    char fn[21];
    sprintf(fn, "newfunction_%8.8zu", i);
    SmartStack::Stack::startFunction(fn);
    SmartStack::Stack::endFunction();
  }
  SmartStack::Stack::endSession();
}

BENCHMARK(bench_vectorRandomLookup);
BENCHMARK(bench_getFunctionPointer);
BENCHMARK(bench_getPointerAndStart);
BENCHMARK(bench_lookupExistingFunction);
BENCHMARK(bench_createFunctionString);
BENCHMARK(bench_lookupNewFunction);

BENCHMARK_MAIN();
