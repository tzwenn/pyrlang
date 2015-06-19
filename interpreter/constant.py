from pyrlang.interpreter.datatypes.number import W_IntObject
from pyrlang.interpreter.datatypes.list import W_NilObject

CONST_0 = W_IntObject(0)
CONST_NIL = W_NilObject()

# the process terminated normally
STATE_TERMINATE = 0 
# the process run out of its reduction steps
# and thus should be suspended and inserted
# to run able queue again
STATE_SWITH = 1 
# the process didn't match any message in its
# message and thus should be suspended and
# removed out of the runable queue
STATE_HANG_UP = 2

PRIORITY_MAXIMUM = 0
PRIORITY_HIGH = 1
PRIORITY_NORMAL = 2
PRIORITY_LOW = 4

# how many times we skip a low priority process
# before we execute it.
LOW_PRIORITY_PROCESS_SKIP_TIMES = 5

# use it to represent an undefined register tuple of (address, tag)
INVALID_REG = (0,0)

# used for jit.loop_unrolling_heuristic hint
UNROLLING_CUTOFF = 5
