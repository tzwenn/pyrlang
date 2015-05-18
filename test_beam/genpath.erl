-module(genpath).
-export([test/1]).

genpath(N) -> genpath(N, 0, "").

%genpath(0, 0, S) -> erlang:display(S);
genpath(0, 0, S) -> 1;
genpath(N, D, S) when D < N, D > 0 ->
	add_left(N, D, S)+add_right(N, D, S);
genpath(N, D, S) when D < N ->
	add_left(N, D, S);
genpath(N, D, S) when D > 0 ->
	add_right(N, D, S).

add_left(N, D, S) -> 
	genpath(N, D+1, [$)|S]).

add_right(N, D, S) ->
	genpath(N-1, D-1, [$(|S]).

test(N) -> genpath(N).
