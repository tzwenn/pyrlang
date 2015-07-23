-module(test_build).
-export([test/0, test/3]).

test() ->
	test(3,7,14).

test(A,B,C) ->
	<<A,B:16,C>>.
