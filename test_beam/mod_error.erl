-module(mod_error).
-import(test_error, [test/0]).
-export([test_call/0]).

test_call() ->
	test_error:test().
