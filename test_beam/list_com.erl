-module(list_com).
-export([test/1]).

test(N) ->
	[E + 1 || E <- lists:seq(1, N)].
