-module(diviter).
-export([test/1, test/0]).

create_n(N) ->
	create_n(N, []).

create_n(0,A) -> A;
create_n(N,A) -> create_n(N-1, [[]|A]).

interative_div2(L) ->
	interative_div2(L, []).

interative_div2([],A) -> A;
interative_div2([H,_|T],A) ->
	interative_div2(T, [H|A]).

test(N) ->
	interative_div2(create_n(N)).

test() ->
	test(200).
