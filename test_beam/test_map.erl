-module(test_map).
-export([test_map/1, test/0]).

test_map(A) ->
	lists:map(fun(N) -> 
					  N + A end, [1]).

test() ->
	test_map(3).
