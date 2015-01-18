-module(test_catch_print).
-export([test/0]).

test() ->
	io:format(catch 5 div 0).
