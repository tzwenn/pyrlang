% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%
% contributed by Isaac Gouy (Erlang novice)
% parallelized by Kevin Scaldeferri

-module(binarytrees).
-export([main/1]).
-export([depth/2]).

% for test
-export([bottomUp/2]).
-export([sumLoop/3]).
-export([itemCheck/1]).
-export([test/2]).

-define(Min,4).

main(N) ->
   Max = lists:max([?Min+2,N]),

   Stretch = Max + 1,
   erlang:display("stretch tree of depth " ++ 
				  integer_to_list(Stretch) ++
				  "\t check: " ++
				  integer_to_list(itemCheck(bottomUp(0, Stretch)))),

   LongLivedTree = bottomUp(0,Max),
   depthLoop(?Min,Max),

   erlang:display("long lived tree of depth " ++
				  integer_to_list(Max) ++
				  "\t check: " ++
				  integer_to_list(itemCheck(LongLivedTree))).

depthLoop(D,M) ->
    Results = lists:map(fun(D_D) ->
								depth(D_D,M)
						end, lists:seq(D, M, 2)),
    lists:foreach(fun(Result) ->
						  erlang:display(integer_to_list(lists:nth(1, Result)) ++
										  "\t trees of depth " ++
										  integer_to_list(lists:nth(2, Result)) ++
										  "\t check: " ++
										  integer_to_list(lists:nth(3, Result)))
                  end,
                  Results).

depth(D,M) ->
    N = 1 bsl (M-D + ?Min),
    [ 2*N, D, sumLoop(N,D,0) ].

sumLoop(0,_,Sum) -> Sum;
sumLoop(N,D,Sum) ->
   sumLoop(N-1,D, Sum + itemCheck(bottomUp(N,D)) + itemCheck(bottomUp(-1*N,D))).

bottomUp(I,0) -> {I, nil, nil};
bottomUp(I,D) -> {I, bottomUp(2*I-1,D-1), bottomUp(2*I,D-1)}.

itemCheck(nil) -> 0;
itemCheck({I,Left,Right}) ->
   LRes = itemCheck(Left),
   RRes = itemCheck(Right),
   Res = I + LRes - RRes,
   %Res = I + itemCheck(Left) - itemCheck(Right),
   %erlang:display(integer_to_list(I) ++ " + " ++ integer_to_list(LRes) ++ " - " ++ integer_to_list(RRes) ++ " = " ++ integer_to_list(Res)),
   Res.

test(A, B) ->
	itemCheck(bottomUp(A,B)).
