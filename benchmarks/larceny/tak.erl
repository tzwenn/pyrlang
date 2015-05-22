-module(tak).
-export([test/0,test/3]).

tak(X,Y,Z) when Y < X ->
	tak(tak(X-1, Y, Z), tak(Y-1, Z, X), tak(Z-1, X, Y));
tak(_,_,Z) -> Z.

test() ->
	test(18, 12, 6).

test(X, Y, Z) ->
	tak(X,Y,Z).

