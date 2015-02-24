-module(test_minus).
-export([test/1]).

test(A) ->
	-3*A.
