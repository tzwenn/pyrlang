-module(test_pi).

-export([test/1, test_s/1,test_s10/1, test_si/1]).

test(N) ->
	test_s(N)/N.

test_s(N) ->
	lists:sum(lists:duplicate(N, math:pi())).

test_s10(N) ->
	lists:sum(lists:duplicate(N, 314159.26535912431251235)).

test_si(N) ->
	lists:sum(lists:duplicate(N, 999)).
