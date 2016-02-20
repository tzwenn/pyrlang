-module(mazefun).
-export([test/2, test/0, run_benchmark/1]).

-define(INITIAL_RANDOM, 0).

even(X) when X >= 0 -> (X band 1) == 0.
odd(X) when X > 0 -> not even(X).

foldr(F, Base, Lst) ->
	FoldrAux = fun (_,[]) -> Base;
				   (Self,[H|T]) -> F(H, Self(Self,T)) end,
	FoldrAux(FoldrAux,Lst).

foldl(F, Base, Lst) ->
	FoldlAux = fun (_, Base, []) -> Base;
			       (Self,Base, [H|T]) -> Self(Self,F(Base, H), T) end,
	FoldlAux(FoldlAux, Base, Lst).

for(Lo, Hi, F) ->
	ForAux = fun (Self,Lo) when Lo < Hi -> [F(Lo)|Self(Self,Lo+1)];
				 (_,_) -> [] end,
	ForAux(ForAux,Lo).

concat(Lists) ->
	foldr(fun(L1,L2) -> L1 ++ L2 end,[],Lists).

list_read([H|_],0) -> H;
list_read([_|T],I) ->
	list_read(T,I-1).

list_write([_|T],0,Val) -> [Val|T];
list_write([H|T],I,Val) ->
	[H|list_write(T,I-1,Val)].

list_remove_pos([_|T], 0) -> T;
list_remove_pos([H|T], I) ->
	[H|list_remove_pos(T, I-1)].

is_duplicate([]) -> false;
is_duplicate([H|T]) ->
	lists:member(H,T) or is_duplicate(T).

make_matrix(N,M,Init) ->
	for(0,N,fun(I) ->
					for(0,M,fun(J) -> Init(I,J) end)
			end).

matrix_read(Mat,I,J) ->
	list_read(list_read(Mat,I),J).

matrix_write(Mat,I,J,Val) ->
	list_write(Mat,I,list_write(list_read(Mat,I),J,Val)).

matrix_size(Mat) ->
	{length(Mat),length(hd(Mat))}.

matrix_map(F,Mat) ->
	lists:map(fun(Lst) -> lists:map(F,Lst) end, Mat).

next_random(CurrentRandom) ->
	(CurrentRandom * 3581 + 12751) rem 131072.

shuffle(Lst) ->
	shuffle_aux(Lst, ?INITIAL_RANDOM).

shuffle_aux([], _) -> [];
shuffle_aux(Lst, CurrentRandom) ->
	NewRandom = next_random(CurrentRandom),
	I = NewRandom rem length(Lst),
	[list_read(Lst, I)|shuffle_aux(list_remove_pos(Lst, I),NewRandom)].

make_maze(N,M) ->
	case not (odd(N) and odd(M)) of
		true -> error;
		false -> 
			Cave = make_matrix(N,M, fun(I,J) ->
											case even(I) and even(J) of
												true -> [I|J];
												false -> false end
									end),
			PossibleHoles = concat(for(0,N,fun(I) ->
												   concat(for(0,M, fun(J) ->
																		   case even(I) == even(J) of
																			   true -> [];
																			   false -> [[I|J]] end
																   end))
										   end)),
			cave_to_maze(pierce_randomly(shuffle(PossibleHoles),Cave)) 
	end.

cave_to_maze(Cave) ->
	matrix_map(fun(X) -> 
					   case X of
						   false -> '*';
						   _ -> '_'
					   end
			   end, Cave).

pierce(Pos, Cave) ->
	I = hd(Pos),
	J = tl(Pos),
	matrix_write(Cave, I, J, Pos).

pierce_randomly([], Cave) -> Cave;
pierce_randomly([Hole|T], Cave) ->
	pierce_randomly(T, try_to_pierce(Hole, Cave)).

try_to_pierce(Pos, Cave) ->
	Ncs = neighboring_cavities(Pos, Cave),
	case is_duplicate(lists:map(fun([H|T]) ->
										matrix_read(Cave, H, T) end, Ncs)) of
		true -> Cave;
		false -> pierce(Pos, foldl(fun(C,Nc) -> change_cavity(C,Nc,Pos) end,
								   Cave, Ncs))
	end.

change_cavity(Cave, Pos, NewCavityId) ->
	I = hd(Pos),
	J = tl(Pos),
	change_cavity_aux(Cave,Pos,NewCavityId,matrix_read(Cave,I,J)).

change_cavity_aux(Cave, Pos, NewCavityId, OldCavityId) ->
	I = hd(Pos),
	J = tl(Pos),
	CavityId = matrix_read(Cave, I, J),
	case CavityId == OldCavityId of
		true -> foldl(fun(C,Nc) -> 
							  change_cavity_aux(C,Nc,NewCavityId,OldCavityId) end, 
					  matrix_write(Cave,I,J,NewCavityId), 
					  neighboring_cavities(Pos, Cave));
		false -> Cave
	end.

neighboring_cavities(Pos, Cave) ->
	Size = matrix_size(Cave),
	N = element(1,Size),
	M = element(2,Size),
	I = hd(Pos),
	J = tl(Pos),
	P1 = case (I > 0) andalso is_list(matrix_read(Cave,I-1,J)) of
			 true -> [[I-1|J]];
			 false -> [] end,
	P2 = case (I < N-1) andalso is_list(matrix_read(Cave,I+1,J)) of
			 true -> [[I+1|J]];
			 false -> [] end,
	P3 = case (J > 0) andalso is_list(matrix_read(Cave,I,J-1)) of
			 true -> [[I|J-1]];
			 false -> [] end,
	P4 = case (J < M-1) andalso is_list(matrix_read(Cave,I,J+1)) of
			 true -> [[I|J+1]];
			 false -> [] end,
	P1++P2++P3++P4.

test() ->
	test(11,11).

test(N,M) ->
	make_maze(N,M).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
