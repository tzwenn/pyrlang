-module(test_display).
-export([test/0]).

test() ->
	Res = erlang:display([1,2,3,4]),
	Res.
