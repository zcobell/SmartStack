# SmartStack
A simple stack tracing and timing library for C++ and Fortran

[![Build Status](https://travis-ci.com/zcobell/SmartStack.svg?branch=master)](https://travis-ci.com/zcobell/SmartStack)
[![codecov](https://codecov.io/gh/zcobell/SmartStack/branch/master/graph/badge.svg)](https://codecov.io/gh/zcobell/SmartStack)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/51020adea56f4c04876262d6ce1eb5bd)](https://www.codacy.com/manual/zachary.cobell/SmartStack?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=zcobell/SmartStack&amp;utm_campaign=Badge_Grade)

# Usage
SmartStack is meant to provide an easy way to generate tracing and timing information from C++ and Fortran codebases with minimal overhead.

## Example Report
Example report that can be generated. Sorted descending by number of function calls. Reports can be sorted by number of calls, duration, or mean duration in either ascending or descending direction by providing arguments to the reporting functions. 
```
Function report for: TestSession
|----------|--------------------------------|-------------|--------------------|-----------------------|
|   Rank   |         Function Name          | [^] Calls   |  [-] Duration (s)  | [-] Mean Duration (s) |
|----------|--------------------------------|-------------|--------------------|-----------------------|
|        1 |                           main |           1 |       0.000000e+00 |          0.000000e+00 |
|        2 |                         Smart1 |           2 |       3.300000e-05 |          1.600000e-05 |
|        3 |                         Smart2 |           5 |       6.000000e-06 |          1.000000e-06 |
|----------|--------------------------------|-------------|--------------------|-----------------------|
```

### Example Tracing
```
[Stack MySession]: main --> Smart1
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart1
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart2
```

## Benchmarking
Benchmarking is conducted using the Google Benchmark library. The benchmark looks at toggling the timer for a function that already exists in the stack and a function that is newly created on a stack. Note that it is expected that the former is the most likely case.
```
2020-03-22 23:08:26
Running ./smartstack_benchmark
Run on (6 X 2304 MHz CPU s)
CPU Caches:
  L1 Data 32 KiB (x6)
  L1 Instruction 32 KiB (x6)
  L2 Unified 256 KiB (x6)
  L3 Unified 16384 KiB (x1)
Load Average: 0.00, 0.02, 0.00
-----------------------------------------------------------------------
Benchmark                             Time             CPU   Iterations
-----------------------------------------------------------------------
bench_lookupExistingFunction        517 ns          514 ns      1237228
bench_lookupNewFunction            1194 ns         1190 ns       815840
```

## C++
The C++ library provides a series of macros so that only minimal code is needed to annotate functions. The code is initialized by beginning a session, and then by using the macros `ADD_SMARTSTACK("functionName")`. When the function goes out of scope, the timer willl automatically be stopped.

### Example Usage
```C++

#include "smartstack.h"

void function1(){
  ADD_SMARTSTACK("function1");
}
void function2(){
  ADD_SMARTSTACK("function2");
}

int main(){

  SmartStack::startSession("MySession");
  ADD_SMARTSTACK("main");


  END_SMARTSTACK();
  SmartStack::printTimingReport();

  return 0;
}

```

## Fortran
The FORTRAN library functions are meant to allow the user to specify a function name at the entry point of their code only. The code will automatically stop counting the function when the created variable goes out of scope.

### Example Usage

```Fortran
MODULE TESTMODULE
  USE SMARTSTACKMODULE
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE MYFUNCTION1()
    IMPLICIT NONE
    TYPE(SMARTSTACK) :: STK
    STK = SMARTSTACK("MYFUNCITON1")
    CALL MYFUNCTION2()
  END SUBROUTINE MYFUNCTION1

  SUBROUTINE MYFUNCTION2()
    IMPLICIT NONE
    TYPE(SMARTSTACK) :: STK
    STK = SMARTSTACK("MYFUNCTION2")
  END SUBROUTINE MYFUNCTION2

END MODULE TESTMODULE

PROGRAM TEST
  USE TESTMODULE
  USE SMARTSTACKMODULE
  IMPLICIT NONE

  TYPE(SMARTSTACK) :: STK

  CALL SMARTSTACK_StartSession("MYSESSION")

  STK = SMARTSTACK("MAIN")

  CALL MYFUNCTION1()
  CALL MYFUNCTION1()

  CALL SMARTSTACK_EndSession()

  CALL SMARTSTACK_PrintTimingReport()

END PROGRAM TEST

```

## Python
Python will eventually be supported via SWIG wrappers
