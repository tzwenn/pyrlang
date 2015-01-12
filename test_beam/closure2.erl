-module(closure2).
-export([test/1]).
-export([test/2]).

test(A) ->
	Fun1 = fun(X) -> X + 1 end,
	Fun1(A).

test(A,B) ->
	Func2 = fun(X,Y) -> X - Y end,
	Func2(A,B).
