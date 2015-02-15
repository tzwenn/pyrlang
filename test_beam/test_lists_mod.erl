-module(test_lists_mod).
-export([test_append/0, test_duplicate/0, test_flatmap/0, test_foldl/0]).

test_append() ->
	lists:append([[1,2,3], [a, b], [4, 5, 6]]).

test_duplicate() ->
	lists:duplicate(5, xx).

test_flatmap() ->
	lists:flatmap(fun(X) -> [X, X] end, [a,b,c]).

test_foldl() ->
	lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).

