-module(mccarthy).
-export([test/1]).

f(N) when N > 100 -> N - 100;
f(N) -> f(f(N+11)).

test(N) -> f(N).
