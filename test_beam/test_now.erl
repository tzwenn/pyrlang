-module(test_now).
-export([test/0]).

test() ->
	erlang:display(now()).
