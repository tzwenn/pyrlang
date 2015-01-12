-module(closure3).
-export([test/1]).
-export([test/2]).
-export([test/3]).

test(A) ->
	Fun1 = fun(X) -> X + 1 end,
	Fun1(A).

test(A,B) ->
	Func2 = fun(X,Y) -> X - Y end,
	Func2(A,B).

test(A,B,C) ->
	Func3 = fun(X,Y,Z) -> X + Y - Z end,
	Func3(A,B,C).
