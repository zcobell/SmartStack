

        MODULE SMARTSTACKMODULE
            USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
            IMPLICIT NONE

            TYPE SMARTSTACK
                PRIVATE
                TYPE(C_PTR) :: ptr
                CONTAINS
                    FINAL :: destructor
            END TYPE SMARTSTACK

            INTERFACE SMARTSTACK
                PROCEDURE :: constructor
            END INTERFACE SMARTSTACK

            INTERFACE
                TYPE(C_PTR) FUNCTION c_createSmartStack(functionName) BIND(C,NAME="addSmartStackFtn") RESULT(ptr)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: functionName
                END FUNCTION c_createSmartStack

                SUBROUTINE c_deleteSmartStack(ptr) BIND(C,NAME="deleteSmartStackFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
                    IMPLICIT NONE
                    TYPE(C_PTR),VALUE,INTENT(IN) :: ptr
                END SUBROUTINE c_deleteSmartStack

                SUBROUTINE c_startSession(session_name) BIND(C,NAME="startSessionFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: session_name
                END SUBROUTINE c_startSession

                SUBROUTINE c_endSession() BIND(C,NAME="endSessionFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_endSession

                SUBROUTINE c_printStack() BIND(C,NAME="printFunctionStackFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_printStack

                SUBROUTINE c_printTimingReport() BIND(C,NAME="printTimingReportFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_printTimingReport

            END INTERFACE

            CONTAINS

                FUNCTION constructor(function_name) RESULT(this)
                    USE,INTRINSIC    :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CHARACTER(*)     :: function_name
                    TYPE(SMARTSTACK) :: this
                    this%ptr = c_createSmartStack(function_name//C_NULL_CHAR)
                END FUNCTION constructor

                SUBROUTINE destructor(this)
                    IMPLICIT NONE
                    TYPE(SMARTSTACK),INTENT(IN) :: this
                    CALL c_deleteSmartStack(this%ptr)
                END SUBROUTINE destructor

                SUBROUTINE SmartStack_startSession(session_name)
                    USE,INTRINSIC    :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CHARACTER(*)     :: session_name
                    CALL c_startSession(session_name//C_NULL_CHAR)
                END SUBROUTINE SmartStack_startSession

                SUBROUTINE SmartStack_endSession()
                    CALL c_endSession()
                END SUBROUTINE SmartStack_endSession

                SUBROUTINE SmartStack_printCurrentStackTrace()
                    IMPLICIT NONE
                    CALL c_printStack()
                END SUBROUTINE SmartStack_printCurrentStackTrace

                SUBROUTINE SmartStack_printTimingReport()
                    IMPLICIT NONE
                    CALL c_printTimingReport()
                END SUBROUTINE SmartStack_printTimingReport


        END MODULE SMARTSTACKMODULE
