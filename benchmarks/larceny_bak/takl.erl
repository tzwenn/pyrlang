-module(takl).
-export([test/0,test/3]).

listn(0) -> [];
listn(N) -> [N|listn(N-1)].

shorterp(_,[]) -> false;
shorterp([],_) -> true;
shorterp([_|Tx],[_|Ty]) -> 
	shorterp(Tx, Ty).

mas(X,Y,Z) ->
	case shorterp(Y,X) of
		true -> mas(mas(tl(X),Y,Z), mas(tl(Y),Z,X), mas(tl(Z),X,Y));
		false -> Z
	end.

test() ->
	test(18, 12, 6).

test(L1, L2, L3) ->
	mas(listn(L1),listn(L2),listn(L3)).
