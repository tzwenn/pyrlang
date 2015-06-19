-module(nqueens).
-export([test/1, test/0, run_benchmark/1]).

nqueens(N) ->
	_1_To = fun(N) ->
					Loop = fun Loop(0, L) -> L;
			   			       Loop(I, L) -> Loop(I-1,[I|L]) end,
					Loop(N, [])
			end,
	IsOK = fun IsOK(_, _, []) -> true;
			   IsOK(Row, Dist, Placed) ->
			   		(not (hd(Placed) == Row + Dist)) andalso
					(not (hd(Placed) == Row - Dist)) andalso
					IsOK(Row, Dist+1, tl(Placed))
		   end,
	My_Try = fun My_Try([],[],Z) -> erlang:display(Z),1;
			     My_Try([],_,_) -> 0;
				 My_Try([H|T],Y,Z) ->
				 				Tmp = case IsOK(H,1,Z) of
											true -> My_Try(T ++ Y, [], [H|Z]);
											false -> 0
									  end,
								Tmp + My_Try(T, [H|Y], Z)
			 end,
	My_Try(_1_To(N), [], []).

test() -> test(8).

test(N) -> nqueens(N).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
