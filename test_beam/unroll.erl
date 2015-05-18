-module(unroll).

-export([test/1]).

unroll(N) when N < 2 -> 1;
unroll(2) -> 2;
unroll(3) -> 3;
unroll(N) ->
	unroll(N - 2) + unroll(N - 3) + unroll(N - 3) + unroll(N - 4).

test(N) -> unroll(N).
