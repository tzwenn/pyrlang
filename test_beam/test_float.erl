-module(test_float).
-export([test/0]).

test() -> 3.1415926 + test2().

test2() -> 1.00000.

