# SmartStack
A simple stack tracing and timing library for C++ and Fortran

[![Build Status](https://travis-ci.com/zcobell/SmartStack.svg?branch=master)](https://travis-ci.com/zcobell/SmartStack)
[![codecov](https://codecov.io/gh/zcobell/SmartStack/branch/master/graph/badge.svg)](https://codecov.io/gh/zcobell/SmartStack)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/51020adea56f4c04876262d6ce1eb5bd)](https://www.codacy.com/manual/zachary.cobell/SmartStack?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=zcobell/SmartStack&amp;utm_campaign=Badge_Grade)

# Usage
SmartStack is meant to provide an easy way to generate tracing and timing information from C++ and Fortran code bases with minimal overhead.

## Example Report
Example report that can be generated. Sorted descending by number of function calls. Reports can be sorted by number of calls, duration, or mean duration in either ascending or descending direction by providing arguments to the reporting functions.


|   Rank   | Function Name | [-] Calls   |  [v]  Local Duration  (us) | [-]  Mean Local Duration (us) |  [-] Local + Child Duration (us) | [-] Mean Local + Child Duration (us) |
|----------|---------------|-------------|----------------------------|-------------------------------|----------------------------------|--------------------------------------|
|        1 |        Smart2 |           3 |                        922 |                           307 |                              922 |                                  307 |
|        2 |        Smart1 |           2 |                        724 |                           362 |                             1470 |                                  735 |
|        3 |          MAIN |           1 |                          9 |                             9 |                             1672 |                                 1672 |



### Example Tracing
Tracing can be done at any point in the code using the tracing function to generate output like below.
```
[Stack MySession]: main --> Smart1
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart1
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart1 --> Smart2
[Stack MySession]: main --> Smart2
```
Entry and exit tracing can be done by specifying the boolean variable to show the stack at object creation for tracing like below. Note that the option is specific to each function.
```
[Stack TestSession]: MAIN --> Smart1: Enter
[Stack TestSession]: MAIN --> Smart1 --> Smart2: Enter
[Stack TestSession]: MAIN --> Smart1 --> Smart2: Return
[Stack TestSession]: MAIN --> Smart1: Return
[Stack TestSession]: MAIN --> Smart1: Enter
[Stack TestSession]: MAIN --> Smart1 --> Smart2: Enter
[Stack TestSession]: MAIN --> Smart1 --> Smart2: Return
[Stack TestSession]: MAIN --> Smart1: Return
[Stack TestSession]: MAIN --> Smart2: Enter
[Stack TestSession]: MAIN --> Smart2: Return
```

## Benchmarking
Benchmarking is conducted using the Google Benchmark library. The benchmark looks at toggling the timer for a random function that already exists in the stack, the same function continuously that already exists, and a function that is newly created on a stack. It is expected that the former is the most likely case.

The relevant tests below are `bench_lookupRandomExistingFunction`, `bench_lookupSameExistingFunction` and `bench_lookupNewFunction`. The random lookup is has a performance of approximately 12.6 million/second for a random function and 14.3 million/second for the same function continuously. Creating a new function has a performance off about 1.6 million/second.

The below tests are conducted using the GNU compiler version 9.2.1 and Abseil tables enabled.
```
2020-03-25 13:56:28
Running ./smartstack_benchmark
Run on (6 X 2304 MHz CPU s)
CPU Caches:
  L1 Data 32 KiB (x6)
  L1 Instruction 32 KiB (x6)
  L2 Unified 256 KiB (x6)
  L3 Unified 16384 KiB (x1)
Load Average: 0.04, 0.17, 0.24
----------------------------------------------------------------------------------------------
Benchmark                                   Time             CPU   Iterations items_per_second
----------------------------------------------------------------------------------------------
bench_lookupRandomExistingFunction       79.3 ns         79.3 ns      8106327       12.6049M/s
bench_lookupSameExistingFunction         69.8 ns         69.8 ns      9615968         14.33M/s
bench_lookupNewFunction                   602 ns          602 ns      1000000       1.65986M/s

```

## C++
The C++ library provides a series of macros so that only minimal code is needed to annotate functions. The code is initialized by beginning a session, and then by using the macros `ADD_SMARTSTACK("functionName")`. When the function goes out of scope, the timer will automatically be stopped.

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
    STK%INITIALIZE = .TRUE.
    CALL MYFUNCTION2()
  END SUBROUTINE MYFUNCTION1

  SUBROUTINE MYFUNCTION2()
    IMPLICIT NONE
    TYPE(SMARTSTACK) :: STK
    STK = SMARTSTACK("MYFUNCTION2")
    STK%INITIALIZE = .TRUE.
  END SUBROUTINE MYFUNCTION2

END MODULE TESTMODULE

PROGRAM TEST
  USE TESTMODULE
  USE SMARTSTACKMODULE
  IMPLICIT NONE

  TYPE(SMARTSTACK) :: STK

  CALL SMARTSTACK_StartSession("MYSESSION")

  STK = SMARTSTACK("MAIN")
  STK%INITIALIZE = .TRUE.

  CALL MYFUNCTION1()
  CALL MYFUNCTION1()

  CALL SMARTSTACK_EndSession()

  CALL SMARTSTACK_PrintTimingReport()

END PROGRAM TEST

```

## Python
Python will eventually be supported via SWIG wrappers
