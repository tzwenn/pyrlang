-module(test_error).
-export([test/0]).

test() ->
	Res = throw_error(),
	Res + 1.

throw_error() ->
	erlang:error(undefined),
	1.
