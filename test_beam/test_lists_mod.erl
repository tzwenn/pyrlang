-module(test_lists_mod).
-export([test_append/0, 
		 test_duplicate/0, 
		 test_flatmap/0, 
		 test_foldl/0, 
		 test_filtermap/0,
		test_mapfoldr/0,
		test_nth/0,
		test_nthtail/0,
		test_reverse/0,
		test_partition/0,
		test_partition2/0,
		test_map/0,
		test_foreach/0,
		test_seq/0]).

test_append() ->
	lists:append([[1,2,3], [a, b], [4, 5, 6]]).

test_duplicate() ->
	lists:duplicate(5, xx).

test_flatmap() ->
	lists:flatmap(fun(X) -> [X, X] end, [a,b,c]).

test_foldl() ->
	lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).

test_filtermap() ->
	lists:filtermap(fun(X) -> 
							case X rem 2 of 0 -> {true, X div 2};
											_ -> false
							end
					end,
					[1,2,3,4,5]).

%% {[2,4,6,8,10],15}
test_mapfoldr() ->
	lists:mapfoldl(fun(X, Sum) -> {2*X, X+Sum} end,
				   0, [1,2,3,4,5]).

%% c
test_nth() ->
	lists:nth(3, [a,b,c,d,e]).

%% [d,e]
test_nthtail() ->
	lists:nthtail(3, [a,b,c,d,e]).

%% {[1,3,5,7], [2,4,6]}
test_partition() ->
	lists:partition(fun(A) -> A rem 2 == 1 end,
					[1,2,3,4,5,6,7]).

%% {[a,b,c,d,e],1,2,3,4]}
test_partition2() ->
	lists:partition(fun(A) -> is_atom(A) end, 
					[a,b,1,c,d,2,3,4,e]).

test_reverse() ->
	lists:reverse([], [a,b]).

test_map() ->
	lists:map(fun(N) -> depth(N) end, [1]).

depth(N) ->
	N + 1.

test_foreach() ->
	lists:foreach(fun(E) -> erlang:display(E) end, [1,2,3,4,5]).

test_seq() ->
	lists:seq(1, 20, 3).
