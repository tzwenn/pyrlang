-module(fibfp).
-export([test/1, test/0, test10/0]).

fibfp(N) when N < 2.0 -> N;
fibfp(N) ->
	fibfp(N-1.0) + fibfp(N-2.0).

test(N) -> fibfp(N).

test() -> test(35.0).
test10() -> test(10.0).
