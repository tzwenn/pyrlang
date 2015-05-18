-module(genpath).
-export([test/1]).

genpath(N) -> genpath(N, 0, "").

genpath(0, 0, S) -> erlang:display(S);
genpath(N, D, S) when D < N, D > 0 ->
	genpath(N-1, D-1, S ++ ")"),
	genpath(N, D+1, S ++ "(");
genpath(N, D, S) when D < N ->
	genpath(N, D+1, S ++ "(");
genpath(N, D, S) when D > 0 ->
	genpath(N-1, D-1, S ++ ")").

test(N) -> genpath(N).
