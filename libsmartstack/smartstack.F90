

        MODULE SMARTSTACKMODULE
            USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
            IMPLICIT NONE
            
            INTEGER,PARAMETER :: SMARTSTACK_SORTASCENDING = 10000
            INTEGER,PARAMETER :: SMARTSTACK_SORTDECENDING = 10001
            INTEGER,PARAMETER :: SMARTSTACK_SORTTIME      = 20000
            INTEGER,PARAMETER :: SMARTSTACK_SORTMEANTIME  = 20001
            INTEGER,PARAMETER :: SMARTSTACK_SORTCALLS     = 20002

            TYPE SMARTSTACK
                LOGICAL,PUBLIC      :: initialize = .TRUE.
                TYPE(C_PTR),PRIVATE :: ptr
                CONTAINS
                    FINAL                 :: destructor
                    PROCEDURE, PASS(THIS) :: init => init_t
            END TYPE SMARTSTACK

            INTERFACE SMARTSTACK
                PROCEDURE :: constructor
            END INTERFACE SMARTSTACK

            INTERFACE
                TYPE(C_PTR) FUNCTION c_createSmartStack(functionName) &
                        BIND(C,NAME="addSmartStackFtn") RESULT(ptr)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: functionName
                END FUNCTION c_createSmartStack
                
                TYPE(C_PTR) FUNCTION c_createSmartStackShowTrace(functionName) &
                        BIND(C,NAME="addSmartStackShowFtn") RESULT(ptr)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: functionName
                END FUNCTION c_createSmartStackShowTrace

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
                
                SUBROUTINE c_printStackMessage(message) BIND(C,NAME="printFunctionStackMessageFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: message
                END SUBROUTINE c_printStackMessage

                SUBROUTINE c_printTimingReport(SORT_TYPE,SORT_ORDER) &
                        BIND(C,NAME="printTimingReportFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT
                    IMPLICIT NONE
                    INTEGER(KIND=C_INT),VALUE :: SORT_TYPE
                    INTEGER(KIND=C_INT),VALUE :: SORT_ORDER
                END SUBROUTINE c_printTimingReport

                SUBROUTINE c_saveTimingReport(FILENAME,SORT_TYPE,SORT_ORDER) &
                        BIND(C,NAME="saveTimingReportFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_CHAR,C_INT
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN)    :: FILENAME
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: SORT_TYPE
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: SORT_ORDER
                END SUBROUTINE c_saveTimingReport
            END INTERFACE

            CONTAINS

                SUBROUTINE init_t(this,function_name)
                    USE,INTRINSIC    :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CLASS(SMARTSTACK),INTENT(INOUT) :: this
                    CHARACTER(*),INTENT(IN)         :: function_name
                END SUBROUTINE init_t

                FUNCTION constructor(function_name,showStack) RESULT(this)
                    USE,INTRINSIC                   :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR,C_BOOL,&
                                                                                C_NULL_CHAR
                    LOGICAL,INTENT(IN),OPTIONAL     :: showStack                                                                            
                    TYPE(SMARTSTACK)                :: this
                    CHARACTER(*),INTENT(IN)         :: function_name
                    LOGICAL                         :: doShowStack
                    this%initialize = .FALSE.
                    IF(PRESENT(showStack))THEN
                        doShowStack = showStack
                    ELSE
                        doShowStack = .FALSE.
                    ENDIF
                    IF(doShowStack)THEN
                        this%ptr = c_createSmartStackShowTrace(function_name//C_NULL_CHAR)
                    ELSE
                        this%ptr = c_createSmartStack(function_name//C_NULL_CHAR)
                    ENDIF
                END FUNCTION constructor

                SUBROUTINE destructor(this)
                    IMPLICIT NONE
                    TYPE(SMARTSTACK),INTENT(IN) :: this
                    IF(.NOT.this%initialize)RETURN
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

                SUBROUTINE SmartStack_printCurrentStack(message)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CHARACTER(*),INTENT(IN),OPTIONAL :: message
                    IF(PRESENT(message))THEN
                        CALL c_printStackMessage(message//C_NULL_CHAR)
                    ELSE
                        CALL c_printStack()
                    ENDIF
                END SUBROUTINE SmartStack_printCurrentStack

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

                SUBROUTINE SmartStack_saveTimingReport(FILENAME,SORT_TYPE,SORT_ORDER)
                    USE,INTRINSIC    :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CHARACTER(*),INTENT(IN)     :: FILENAME
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
                    CALL c_saveTimingReport(FILENAME//C_NULL_CHAR,F_SORT_TYPE,F_SORT_ORDER)
                END SUBROUTINE SmartStack_saveTimingReport

        END MODULE SMARTSTACKMODULE
