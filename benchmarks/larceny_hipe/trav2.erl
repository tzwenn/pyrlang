-module(trav2).

node_sons(Node) -> element(2, Node).

node_mark(Node) -> element(11, Node).

node_mark_set(Node, V) ->
	setelement(11, Node, V).

iter_node({Head, Parent, Sons, Sn, E1, E2, E3, E4, E5, E6, Mark}, NewMark) ->
	{Head, Parent, Sons, Sn, not E1, not E2, not E3, not E4, not E5, not E6, NewMark}.

%create_structure(N) ->
	%create_structure(0, make_node(0)).

%create_structure(Sn, A, [P|T]) ->
	%NewP = [P|A],



make_node(Sn)->
	{node, [], [], Sn, false, false, false, false, false, false, false}

travers(Node, Mark) when node_mark(Node) == Mark -> false;
travers(Node, Mark) ->
	Node1 = iter_node(Node, Mark),
	Sons = node_sons(Node1),
	travers_sons(Sons, Mark).

travers_sons([], _) -> false;
travers_sons([Node|NextSons], Mark) ->
	travers(Node, Mark),
	travers_sons(NextSons, Mark).
