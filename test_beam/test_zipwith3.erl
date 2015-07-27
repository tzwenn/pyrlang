-module(test_zipwith3).
-export([test/1, seq/1]).

test(N) ->
	lists:zipwith(fun (X, Y) -> X * Y end,
				  lists:seq(1, N),
				  lists:duplicate(N, 42)).

seq(N) ->
	lists:seq(1, N).
