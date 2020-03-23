
        MODULE sample
            USE SMARTSTACKMODULE
            IMPLICIT NONE

            CONTAINS 

            SUBROUTINE Smart1
                IMPLICIT NONE
                TYPE(SMARTSTACK) :: ss
                ss = SmartStack("Smart1")
                CALL SmartStack_printCurrentStackTrace()
                CALL SMART2()
                CALL SMART2()
            END SUBROUTINE Smart1
            
            SUBROUTINE Smart2
                IMPLICIT NONE
                TYPE(SMARTSTACK) :: ss
                ss = SmartStack("Smart2")
                CALL SmartStack_printCurrentStackTrace()
            END SUBROUTINE Smart2

        END MODULE sample


        PROGRAM smartstack_test
            USE sample
            IMPLICIT NONE
            TYPE(SMARTSTACK) :: ss

            CALL SmartStack_StartSession("TestSession")
            ss = SmartStack("main")

            CALL SMART1()
            CALL SMART1()
            CALL SMART2()

            CALL SmartStack_printTimingReport()
            CALL SmartStack_saveTimingReport("report.txt",SmartStack_SortCalls,SmartStack_SortAscending)

        END PROGRAM smartstack_test
