#define ADD_SMARTSTACK(fn, show) TYPE(SmartStack) :: s___; \
  s___ = SmartStack(fn,show);    \
  s___%initialize=.TRUE.;
