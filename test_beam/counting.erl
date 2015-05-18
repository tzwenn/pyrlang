-module(counting).
-export([test/1]).

counting(N) when N < 0 -> 0;
counting(0) -> 1;
counting(N) ->
	counting(N-1) + counting(N-2) + counting(N-3).

test(N) -> counting(N).
