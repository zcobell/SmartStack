
        MODULE SMARTSTACKMODULE
            USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_LONG_LONG
            IMPLICIT NONE

            INTEGER,PARAMETER :: SMARTSTACK_SORTASCENDING = 10000
            INTEGER,PARAMETER :: SMARTSTACK_SORTDECENDING = 10001

            INTEGER,PARAMETER :: SMARTSTACK_SORTTIME      = 20000
            INTEGER,PARAMETER :: SMARTSTACK_SORTMEANTIME  = 20001
            INTEGER,PARAMETER :: SMARTSTACK_SORTCALLS     = 20002

            INTEGER,PARAMETER :: SMARTSTACK_MICROSECONDS  = 30000
            INTEGER,PARAMETER :: SMARTSTACK_MILLISECONDS  = 30001
            INTEGER,PARAMETER :: SMARTSTACK_SECONDS       = 30002
            INTEGER,PARAMETER :: SMARTSTACK_MINUTES       = 30003
            INTEGER,PARAMETER :: SMARTSTACK_HOURS         = 30004

            INTEGER,PARAMETER :: SMARTSTACK_TABLE         = 40000
            INTEGER,PARAMETER :: SMARTSTACK_CSV           = 40001

            TYPE SMARTSTACK
                LOGICAL,PUBLIC      :: initialize = .TRUE.
                INTEGER(C_LONG_LONG),PRIVATE :: ptr
                CONTAINS
                    FINAL                 :: destructor
                    PROCEDURE, PASS(THIS) :: init => init_t
            END TYPE SMARTSTACK

            INTERFACE SMARTSTACK
                PROCEDURE :: constructor
            END INTERFACE SMARTSTACK

            INTERFACE
                INTEGER(C_LONG_LONG) FUNCTION c_createSmartStack(functionName) &
                        BIND(C,NAME="addSmartStackFtn") RESULT(ptr)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_LONG_LONG,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: functionName
                END FUNCTION c_createSmartStack

                INTEGER(C_LONG_LONG) FUNCTION c_createSmartStackShowTrace(functionName) &
                        BIND(C,NAME="addSmartStackShowFtn") RESULT(ptr)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_LONG_LONG,C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: functionName
                END FUNCTION c_createSmartStackShowTrace

                SUBROUTINE c_deleteSmartStack(ptr) BIND(C,NAME="deleteSmartStackFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_LONG_LONG
                    IMPLICIT NONE
                    INTEGER(C_LONG_LONG),VALUE,INTENT(IN) :: ptr
                END SUBROUTINE c_deleteSmartStack

                SUBROUTINE c_startSession(session_name,processorId,proc0ToScreen) BIND(C,NAME="startSessionFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT,C_CHAR,C_BOOL
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN)     :: session_name
                    INTEGER(KIND=C_INT),VALUE,INTENT(IN)  :: processorId
                    LOGICAL(KIND=C_BOOL),VALUE,INTENT(IN) :: proc0ToScreen
                END SUBROUTINE c_startSession

                SUBROUTINE c_startSessionLog(session_name,processorId,proc0ToScreen,logFile) BIND(C,NAME="startSessionLogFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_CHAR,C_INT,C_BOOL
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN)     :: session_name
                    CHARACTER(KIND=C_CHAR),INTENT(IN)     :: logFile
                    INTEGER(KIND=C_INT),VALUE,INTENT(IN)  :: processorId
                    LOGICAL(KIND=C_BOOL),VALUE,INTENT(IN) :: proc0ToScreen
                END SUBROUTINE c_startSessionLog

                SUBROUTINE c_endSession() BIND(C,NAME="endSessionFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_endSession

                SUBROUTINE c_printStack() BIND(C,NAME="printCurrentStackFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_printStack

                SUBROUTINE c_printStackMessage(message) BIND(C,NAME="printCurrentStackMessageFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: message
                END SUBROUTINE c_printStackMessage

                SUBROUTINE c_printCurrentFunction() BIND(C,NAME="printCurrentFunctionFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_printCurrentFunction

                SUBROUTINE c_printCurrentFunctionMessage(message) BIND(C,NAME="printCurrentFunctionMessageFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_CHAR
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN) :: message
                END SUBROUTINE c_printCurrentFunctionMessage

                SUBROUTINE c_printTimingReport(TIME_UNITS,SORT_TYPE,SORT_ORDER) &
                        BIND(C,NAME="printTimingReportFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_INT
                    IMPLICIT NONE
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: TIME_UNITS
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: SORT_TYPE
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: SORT_ORDER
                END SUBROUTINE c_printTimingReport

                SUBROUTINE c_saveTimingReport(FILENAME,TIME_UNITS,SORT_TYPE,SORT_ORDER,OUTPUT_FORMAT) &
                        BIND(C,NAME="saveTimingReportFtn")
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_CHAR,C_INT
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN)    :: FILENAME
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: TIME_UNITS
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: SORT_TYPE
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: SORT_ORDER
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: OUTPUT_FORMAT
                END SUBROUTINE c_saveTimingReport

                SUBROUTINE c_getCurrentStack() BIND(C,NAME="getCurrentStackFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_getCurrentStack

                SUBROUTINE c_getCurrentFunction() BIND(C,NAME="getCurrentFunctionFtn")
                    IMPLICIT NONE
                END SUBROUTINE c_getCurrentFunction


            END INTERFACE

            CHARACTER(:),ALLOCATABLE,PRIVATE :: c_string_buffer
            INTEGER,PRIVATE                  :: c_string_length

            CONTAINS

                SUBROUTINE c2f_copyStringToFortran(c_string,c_string_len) BIND(C,NAME="c2f_copyStringToFortran")
                    USE,INTRINSIC                        :: ISO_C_BINDING,ONLY:C_CHAR,C_INT
                    IMPLICIT NONE
                    CHARACTER(KIND=C_CHAR),INTENT(IN)    :: c_string(*)
                    INTEGER(KIND=C_INT),INTENT(IN),VALUE :: c_string_len
                    INTEGER                              :: I
                    IF(ALLOCATED(c_string_buffer))DEALLOCATE(c_string_buffer)
                    c_string_length = c_string_len
                    ALLOCATE(CHARACTER(c_string_length) :: c_string_buffer)
                    FORALL(i=1:c_string_length)c_string_buffer(I:I) = c_string(I)
                END SUBROUTINE c2f_copyStringToFortran

                SUBROUTINE init_t(this,function_name)
                    USE,INTRINSIC    :: ISO_C_BINDING,ONLY:C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CLASS(SMARTSTACK),INTENT(INOUT) :: this
                    CHARACTER(*),INTENT(IN)         :: function_name
                END SUBROUTINE init_t

                FUNCTION constructor(function_name,showStack) RESULT(this)
                    USE,INTRINSIC                   :: ISO_C_BINDING,ONLY:C_CHAR,C_BOOL,&
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

                SUBROUTINE SmartStack_startSession(session_name, processorId, proc0ToScreen, logFile)
                    USE,INTRINSIC                    :: ISO_C_BINDING,ONLY:C_NULL_CHAR,C_BOOL,C_INT
                    IMPLICIT NONE
                    CHARACTER(*),OPTIONAL,INTENT(IN)  :: session_name
                    CHARACTER(*),OPTIONAL,INTENT(IN)  :: logFile
                    INTEGER,OPTIONAL,INTENT(IN)       :: processorId
                    LOGICAL,OPTIONAL,INTENT(IN)       :: proc0ToScreen
                    INTEGER(KIND=C_INT)               :: f_processorId
                    LOGICAL(KIND=C_BOOL)              :: f_proc0ToScreen
                    IF(PRESENT(processorId))THEN
                        f_processorId = processorId
                    ELSE
                        f_processorId = -1
                    ENDIF
                    IF(PRESENT(proc0ToScreen))THEN
                        f_proc0ToScreen = proc0ToScreen
                    ELSE
                        f_proc0ToScreen = .FALSE.
                    ENDIF
                    IF(PRESENT(logFile))THEN
                        CALL c_startSessionLog(session_name//C_NULL_CHAR,f_processorId,&
                            f_proc0ToScreen,logFile//C_NULL_CHAR)
                    ELSE
                        CALL c_startSession(session_name//C_NULL_CHAR,f_processorId,f_proc0ToScreen)
                    ENDIF
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

                SUBROUTINE SmartStack_printCurrentFunction(message)
                    USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CHARACTER(*),INTENT(IN),OPTIONAL :: message
                    IF(PRESENT(message))THEN
                        CALL c_printCurrentFunctionMessage(message//C_NULL_CHAR)
                    ELSE
                        CALL c_printCurrentFunction()
                    ENDIF
                END SUBROUTINE SmartStack_printCurrentFunction

                SUBROUTINE SmartStack_getCurrentStack(buffer)
                    IMPLICIT NONE
                    CHARACTER(*),INTENT(OUT) :: buffer
                    CALL c_getCurrentStack()
                    IF(LEN(buffer).LT.LEN(c_string_buffer))THEN
                        WRITE(*,'(A)') "SmartStack Error: String buffer overflow detected."//&
                                       " Increase buffer size"
                        buffer = ""
                        RETURN
                    ENDIF
                    buffer = c_string_buffer
                    DEALLOCATE(c_string_buffer)
                END SUBROUTINE SmartStack_getCurrentStack

                SUBROUTINE SmartStack_getCurrentFunction(buffer)
                    IMPLICIT NONE
                    CHARACTER(*),INTENT(OUT) :: buffer
                    CALL c_getCurrentFunction()
                    IF(LEN(buffer).LT.LEN(c_string_buffer))THEN
                        WRITE(*,'(A)') "SmartStack Error: String buffer overflow detected."//&
                                       " Increase buffer size"
                        buffer = ""
                        RETURN
                    ENDIF
                    buffer = c_string_buffer
                    DEALLOCATE(c_string_buffer)
                END SUBROUTINE SmartStack_getCurrentFunction

                SUBROUTINE SmartStack_printTimingReport(UNIT_TYPE,SORT_TYPE,SORT_ORDER)
                    IMPLICIT NONE
                    INTEGER,OPTIONAL,INTENT(IN) :: UNIT_TYPE
                    INTEGER,OPTIONAL,INTENT(IN) :: SORT_TYPE
                    INTEGER,OPTIONAL,INTENT(IN) :: SORT_ORDER
                    INTEGER                     :: F_UNIT_TYPE
                    INTEGER                     :: F_SORT_TYPE
                    INTEGER                     :: F_SORT_ORDER
                    IF(PRESENT(UNIT_TYPE))THEN
                        F_UNIT_TYPE = UNIT_TYPE
                    ELSE
                        F_UNIT_TYPE = SMARTSTACK_MILLISECONDS
                    ENDIF
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
                    CALL c_printTimingReport(F_UNIT_TYPE,F_SORT_TYPE,F_SORT_ORDER)
                END SUBROUTINE SmartStack_printTimingReport

                SUBROUTINE SmartStack_saveTimingReport(FILENAME,UNIT_TYPE,SORT_TYPE,SORT_ORDER,OUTPUT_FORMAT)
                    USE,INTRINSIC    :: ISO_C_BINDING,ONLY:C_CHAR,C_NULL_CHAR
                    IMPLICIT NONE
                    CHARACTER(*),INTENT(IN)     :: FILENAME
                    INTEGER,OPTIONAL,INTENT(IN) :: UNIT_TYPE
                    INTEGER,OPTIONAL,INTENT(IN) :: SORT_TYPE
                    INTEGER,OPTIONAL,INTENT(IN) :: SORT_ORDER
                    INTEGER,OPTIONAL,INTENT(IN) :: OUTPUT_FORMAT
                    INTEGER                     :: F_UNIT_TYPE
                    INTEGER                     :: F_SORT_TYPE
                    INTEGER                     :: F_SORT_ORDER
                    INTEGER                     :: F_OUTPUT_FORMAT
                    IF(PRESENT(UNIT_TYPE))THEN
                        F_UNIT_TYPE = UNIT_TYPE
                    ELSE
                        F_UNIT_TYPE = SMARTSTACK_MILLISECONDS
                    ENDIF
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
                    IF(PRESENT(OUTPUT_FORMAT))THEN
                        F_OUTPUT_FORMAT = OUTPUT_FORMAT
                    ELSE
                        F_OUTPUT_FORMAT = SMARTSTACK_TABLE
                    ENDIF
                    CALL c_saveTimingReport(FILENAME//C_NULL_CHAR,F_UNIT_TYPE,F_SORT_TYPE,F_SORT_ORDER,F_OUTPUT_FORMAT)
                END SUBROUTINE SmartStack_saveTimingReport

        END MODULE SMARTSTACKMODULE
