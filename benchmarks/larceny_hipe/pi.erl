-module(pi).
-export([test/0, test/3, run_benchmark/1]).

exp(_,0) -> 1;
exp(X,N) when N rem 2 == 1 ->
	Part = exp(X, (N-1) div 2),
	X * Part * Part;
exp(X,N) ->
	Part = exp(X, N div 2),
	Part * Part.

width(X) ->
	Fun = fun (_,I,N) when X < N -> I;
			  (Self,I,N) -> Self(Self, I+1, N*2) end,
	Fun(Fun, 0, 1).

root(X, Y) ->
	Fun = fun (Self,G) ->
				A = exp(G, Y-1),
				B = A * Y,
				C = A * (Y-1),
				D = (X + G*C) div B,
				case D < G of
					true -> Self(Self,D);
					false -> G
				end
		   end,
	Fun(Fun, exp(2, (width(X)+Y-1) div Y)).

square_root(X) -> root(X,2).

quartic_root(X) -> root(X,4).

square(X) -> X * X.

pi_brent_salamin(NbDigits) ->
	One = exp(10, NbDigits),
	Fun = fun (_,A,B,T,_) when A == B -> square(A+B) div (4 * T);
			  (Self,A,B,T,X) -> NewA = (A+B) div 2,
							   Self(Self,NewA, square_root(A*B), T - (X * square(NewA-A) div One), 2*X) 
							   end,
	Fun(Fun, One, square_root(square(One) div 2), One div 4, 1).

pi_borwein2(NbDigits) ->
	One = exp(10, NbDigits),
	One2 = square(One),
	One4 = square(One2),
	Sqrt2 = square_root(One2 * 2),
	Qurt2 = quartic_root(One4 * 2),
	Fun = fun (Self,X,Y,P) ->
				   NewP = P * (X + One) div (Y + One),
				   case X == One of
					   true -> NewP;
					   false ->
						   SqrtX = square_root(One * X),
						   Self(Self,One*(X+One) div (2 * SqrtX),
								One*(X*Y+One2) div ((Y+One) * SqrtX),
								NewP)
				   end
		   end,
	Fun(Fun,One*(One+Sqrt2) div (2 * Qurt2), Qurt2, 2*One+Sqrt2).

pi_borwein4(NbDigits) ->
	One = exp(10, NbDigits),
	One2 = square(One),
	One4 = square(One2),
	Sqrt2 = square_root(One2 * 2),
	Fun = fun (_,0,A,_) -> One2 div A;
			   (Self,Y,A,X) ->
						   T1 = quartic_root(One4-square(square(Y))),
						   T2 = One*(One-T1) div (One+T1),
						   T3 = square(square(One+T2) div One) div One,
						   T4 = One + T2 + square(T2) div One,
						   Self(Self, T2, (T3 * A - X * T2 * T4) div One, 4*X)
		   end,
	Fun(Fun, Sqrt2-One, 6*One-4*Sqrt2, 8).

pies(N,M,_) when M < N -> [];
pies(N,M,S) ->
	Bs = pi_brent_salamin(N),
	B2 = pi_borwein2(N),
	B4 = pi_borwein4(N),
	[[B2, Bs-B2,B4-B2]|pies(N+S, M, S)].

test() -> 
	test(50, 500, 50).

test(N,M,S) -> pies(N,M,S).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
