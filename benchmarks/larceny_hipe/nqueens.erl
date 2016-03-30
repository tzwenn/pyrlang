-module(nqueens).
-export([test/1, test/0, run_benchmark/1]).

nqueens(N) ->
	_1_To = fun(N) ->
					Loop = fun (_, 0, L) -> L;
			   			       (Self, I, L) -> Self(Self,I-1,[I|L]) end,
					Loop(Loop, N, [])
			end,
	IsOK = fun (_, _, _, []) -> true;
			   (Self, Row, Dist, Placed) ->
			   		(not (hd(Placed) == Row + Dist)) andalso
					(not (hd(Placed) == Row - Dist)) andalso
					Self(Self, Row, Dist+1, tl(Placed))
		   end,
	My_Try = fun (_,[],[],Z) -> erlang:display(Z),1;
			     (_,[],_,_) -> 0;
				 (Self,[H|T],Y,Z) ->
				 				Tmp = case IsOK(IsOK, H,1,Z) of
											true -> Self(Self, T ++ Y, [], [H|Z]);
											false -> 0
									  end,
								Tmp + Self(Self, T, [H|Y], Z)
			 end,
	My_Try(My_Try, _1_To(N), [], []).

test() -> test(8).

test(N) -> nqueens(N).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
