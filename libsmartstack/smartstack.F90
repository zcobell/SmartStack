

        MODULE SMARTSTACKMODULE
            USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
            IMPLICIT NONE
            
            INTEGER,PARAMETER :: SMARTSTACK_SORTASCENDING = 10000
            INTEGER,PARAMETER :: SMARTSTACK_SORTDECENDING = 10001
            INTEGER,PARAMETER :: SMARTSTACK_SORTTIME      = 20000
            INTEGER,PARAMETER :: SMARTSTACK_SORTMEANTIME  = 20001
            INTEGER,PARAMETER :: SMARTSTACK_SORTCALLS     = 20002

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

                SUBROUTINE c_printTimingReport(SORT_TYPE,SORT_ORDER) BIND(C,NAME="printTimingReportFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT
                    IMPLICIT NONE
                    INTEGER(C_INT),VALUE :: SORT_TYPE
                    INTEGER(C_INT),VALUE :: SORT_ORDER
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

                SUBROUTINE SmartStack_printTimingReport(SORT_TYPE,SORT_ORDER)
                    IMPLICIT NONE
                    INTEGER,OPTIONAL,INTENT(IN) :: SORT_TYPE
                    INTEGER,OPTIONAL,INTENT(IN) :: SORT_ORDER
                    INTEGER                     :: F_SORT_TYPE
                    INTEGER                     :: F_SORT_ORDER
                    IF(PRESENT(SORT_TYPE))THEN
                        F_SORT_TYPE = SORT_TYPE
                    ELSE
                        F_SORT_TYPE = SMARTSTACK_SORTTIME
                    ENDIF
                    IF(PRESENT(SORT_ORDER))THEN
                        F_SORT_ORDER = SORT_ORDER
                    ELSE
                        F_SORT_ORDER = SMARTSTACK_SORTDECENDING
                    ENDIF
                    CALL c_printTimingReport(F_SORT_TYPE,F_SORT_ORDER)
                END SUBROUTINE SmartStack_printTimingReport


        END MODULE SMARTSTACKMODULE
