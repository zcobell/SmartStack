
        MODULE sample
            USE SMARTSTACKMODULE
            IMPLICIT NONE

            CONTAINS 

            SUBROUTINE Smart1
                IMPLICIT NONE
                TYPE(SMARTSTACK),ALLOCATABLE :: ss
                CHARACTER(200) :: fn
                CHARACTER(50)  :: fn2
                ss = addSmartStack("Smart1")
                CALL SmartStack_getCurrentFunction(fn)
                WRITE(*,'(2A)') "Manually getting current function name: ",TRIM(fn)
                CALL SmartStack_getCurrentStack(fn2)
                WRITE(*,'(2A)') "Checking for buffer overflow: ",TRIM(fn2)
                CALL SmartStack_printCurrentFunction("Hello from Smart1")
                CALL SMART2()
            END SUBROUTINE Smart1
            
            SUBROUTINE Smart2
                IMPLICIT NONE
                TYPE(SMARTSTACK),ALLOCATABLE :: ss
                CHARACTER(200) :: fn
                CHARACTER(50)  :: fn2

                ss = addSmartStack("Smart2")

                CALL SmartStack_getCurrentStack(fn)
                WRITE(*,'(2A)') "Manually getting current stack: ",TRIM(fn)
                CALL SmartStack_getCurrentStack(fn2)
                WRITE(*,'(2A)') "Checking for buffer overflow: ",TRIM(fn2)
                CALL SmartStack_printCurrentStack("Hello from Smart2 subroutine")
            END SUBROUTINE Smart2

        END MODULE sample


        PROGRAM smartstack_test
            USE sample
            IMPLICIT NONE
            TYPE(SMARTSTACK),ALLOCATABLE :: ss

            CALL SmartStack_StartSession("TestSession")
            ss = addSmartStack("Main")

            CALL SMART1()
            CALL SMART1()
            CALL SMART2()

            CALL SmartStack_EndSession()

            CALL SmartStack_printTimingReport()

            CALL SmartStack_printTimingReport(SmartStack_Microseconds,&
                        SMARTSTACK_SORTCALLS,SMARTSTACK_SORTASCENDING)
            CALL SmartStack_saveTimingReport("report.txt",SmartStack_Milliseconds, &
                        SmartStack_SortCalls,SmartStack_SortAscending)
            CALL SmartStack_saveTimingReport("report.csv",SmartStack_Seconds,&
                        SmartStack_SortCalls,SmartStack_SortAscending,SmartStack_CSV)

        END PROGRAM smartstack_test
