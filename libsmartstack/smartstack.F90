

        MODULE SMARTSTACKMODULE
            USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
            IMPLICIT NONE

            TYPE SMARTSTACK
                TYPE(C_PTR) :: ptr
                CONTAINS
                    FINAL :: destructor
            END TYPE SMARTSTACK

            INTERFACE SMARTSTACK
                PROCEDURE :: constructor
            END INTERFACE SMARTSTACK

            CONTAINS

                TYPE(C_PTR) FUNCTION c_createSmartStack(functionName) BIND(C,NAME="addSmartStack_") RESULT(ptr)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(C_CHAR) :: functionName
                END FUNCTION c_createSmartStack

                FUNCTION constructor(function_name) RESULT(this)
                    IMPLICIT NONE
                    CHARACTER(*) :: function_name
                    TYPE(SMARTSTACK) :: this
                    this%ptr = c_createSmartStack(function_name)
                    WRITE(*,*) "Constructor works"
                END FUNCTION constructor

                SUBROUTINE destructor(this)
                    IMPLICIT NONE
                    TYPE(SMARTSTACK),INTENT(INOUT) :: this
                    WRITE(*,*) "Destructor works"
                END SUBROUTINE destructor


        END MODULE SMARTSTACKMODULE
