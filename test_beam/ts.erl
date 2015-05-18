-module(ts).
-export([test/1]).

ts(0) -> 1;
ts(1) -> 1;
ts(N) -> ts(N-1) + ts(N-2).

test(N) -> ts(N).
