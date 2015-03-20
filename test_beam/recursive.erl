-module(recursive).
-export([test/1]).

test(0) -> 1;
test(1) -> 1;
test(N) ->
	test(N - 1) + test(N - 2).
