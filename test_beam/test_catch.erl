-module(test_catch).
-export([test/0]).

test() ->
	catch 5 div 0.
