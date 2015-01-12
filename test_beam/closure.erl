-module(closure).
-export([test/1]).

test(A) ->
	Fun1 = fun(X) -> X + 1 end,
	Fun1(A).
