#define START_SMARTSTACK(session_name, function_name, show_stack) \
    TYPE(SmartStack) :: smartstack___; \
    CALL SmartStack_StartSession(session_name); \
    smartstack___ = SmartStack(function_name, show_stack); \
    smartstack___%initialize=.TRUE.;

#define ADD_SMARTSTACK(function_name, show_stack) \
    TYPE(SmartStack) :: smartstack___; \
    smartstack___ = SmartStack(function_name, show_stack); \
    smartstack___%initialize=.TRUE.;
