
        MODULE sample
            USE SMARTSTACKMODULE
            IMPLICIT NONE

            CONTAINS 

            SUBROUTINE Smart1
                IMPLICIT NONE
                TYPE(SMARTSTACK) :: ss
                ss = SmartStack("Smart1")
                WRITE(*,*) ss%ptr
            END SUBROUTINE Smart1

        END MODULE sample


        PROGRAM smartstack_test
            USE sample
            IMPLICIT NONE

            WRITE(*,*) "About to call"
            CALL SMART1()
            WRITE(*,*) "Returned from call"

        END PROGRAM smartstack_test
