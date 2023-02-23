#include "smartstackf.h"

        MODULE sample
            USE SMARTSTACKMODULE
            IMPLICIT NONE

            CONTAINS 

            SUBROUTINE Smart1
                IMPLICIT NONE
                ADD_SMARTSTACK("Smart1",.TRUE.)
                CALL SmartStack_printCurrentFunction("Hello from Smart1")
                CALL SMART2()
            END SUBROUTINE Smart1
            
            SUBROUTINE Smart2
                IMPLICIT NONE
                ADD_SMARTSTACK("Smart2_ALongFunctionName",.TRUE.)
                CALL SmartStack_printCurrentStack("Hello from Smart2 subroutine")
            END SUBROUTINE Smart2

        END MODULE sample


        PROGRAM smartstack_test
            USE sample
            IMPLICIT NONE
            TYPE(SmartStack) :: stack
            CALL SmartStack_StartSession("MyProgram")
            stack = SmartStack("MAIN", .TRUE.)
            stack%initialize = .TRUE.

            CALL SMART1()
            CALL SMART1()
            CALL SMART2()

            CALL SmartStack_EndSession()

            CALL SmartStack_printTimingReport()

            CALL SmartStack_printTimingReport(SMARTSTACK_MICROSECONDS)

            CALL SmartStack_printTimingReport(SMARTSTACK_MILLISECONDS,SMARTSTACK_SORTCALLS,SMARTSTACK_SORTASCENDING)

            CALL SmartStack_saveTimingReport("report.txt",SmartStack_SortCalls,SmartStack_SortAscending)
            CALL SmartStack_saveTimingReport("report.csv",SmartStack_SortCalls,SmartStack_SortAscending,SmartStack_CSV)

        END PROGRAM smartstack_test
