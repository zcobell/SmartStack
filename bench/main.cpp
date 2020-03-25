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
std::vector<std::string> functionList(c_functionSize);
std::vector<int> intList(c_functionSize);

void initializer() {
  SmartStack::Stack::startSession("Benchmark");
  SmartStack::Stack::startFunction("myfunction");
  SmartStack::Stack::endFunction();
  for (size_t i = 0; i < c_functionSize; ++i) {
    char fn[20];
    sprintf(fn, "function_%8.8zu", i);
    functionList[i] = fn;
    intList[i] = i;
  }
}

static void bench_vectorRandomLookup(benchmark::State& state) {
  while (state.KeepRunning()) {
    benchmark::DoNotOptimize(functionList[rand() % c_functionSize]);
  }
  state.SetItemsProcessed(state.iterations());
}

static void bench_compareStrings(benchmark::State& state) {
  while (state.KeepRunning()) {
    benchmark::DoNotOptimize(functionList[rand() % c_functionSize] ==
                             functionList[rand() % c_functionSize]);
  }
  state.SetItemsProcessed(state.iterations());
}

static void bench_compareInts(benchmark::State& state) {
  while (state.KeepRunning()) {
    benchmark::DoNotOptimize(intList[rand() % c_functionSize] ==
                             intList[rand() % c_functionSize]);
  }
  state.SetItemsProcessed(state.iterations());
}

static void bench_getFunctionPointer(benchmark::State& state) {
  while (state.KeepRunning()) {
    benchmark::DoNotOptimize(SmartStack::Stack::get().getFunctionPointer(
        functionList[rand() % c_functionSize]));
  }
  state.SetItemsProcessed(state.iterations());
}

static void bench_getPointerAndStart(benchmark::State& state) {
  while (state.KeepRunning()) {
    Function* f = SmartStack::Stack::get().getFunctionPointer(
        functionList[rand() % c_functionSize]);
    f->startFunction();
  }
  state.SetItemsProcessed(state.iterations());
}

static void bench_lookupRandomExistingFunction(benchmark::State& state) {
  while (state.KeepRunning()) {
    SmartStack::Stack::startFunction(functionList[rand() % c_functionSize]);
    SmartStack::Stack::endFunction();
  }
  state.SetItemsProcessed(state.iterations());
}

static void bench_lookupSameExistingFunction(benchmark::State& state) {
  while (state.KeepRunning()) {
    SmartStack::Stack::startFunction("function_00010000");
    SmartStack::Stack::endFunction();
  }
  state.SetItemsProcessed(state.iterations());
}
static void bench_createFunctionString(benchmark::State& state) {
  size_t i = 0;
  while (state.KeepRunning()) {
    i++;
    char fn[21];
    benchmark::DoNotOptimize(sprintf(fn, "newfunction_%8.8zu", i));
  }
  state.SetItemsProcessed(state.iterations());
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
  state.SetItemsProcessed(state.iterations());
}

BENCHMARK(bench_lookupRandomExistingFunction);
BENCHMARK(bench_lookupSameExistingFunction);
BENCHMARK(bench_lookupNewFunction);
BENCHMARK(bench_vectorRandomLookup);
BENCHMARK(bench_compareStrings);
BENCHMARK(bench_compareInts);
BENCHMARK(bench_getFunctionPointer);
BENCHMARK(bench_getPointerAndStart);
BENCHMARK(bench_createFunctionString);


int main(int argc, char** argv) {
  initializer();
  ::benchmark::Initialize(&argc, argv);
  ::benchmark::RunSpecifiedBenchmarks();
}
