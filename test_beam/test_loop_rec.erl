-module(test_loop_rec).
-import(reverse, [seq/1, start/1, reverse/1]).
-export([test/2, worker/1, busy_worker/0]).

busy_worker() ->
	receive
		stop ->
			true;
		From ->
			From ! once_more,
			busy_worker()
	end.

worker(Pid) ->
	Pid ! self(),
	receive
		once_more ->
			worker(Pid);
		stop ->
			true
	end.

spawn_many(0, Pids, _, _, _) ->
	Pids;
spawn_many(N, Pids, Mod, Func, Args) ->
	Pid = spawn(Mod, Func, Args),
	spawn_many(N-1, [Pid|Pids], Mod, Func, Args).

spawn_many(N, Mod, Func, Args) ->
	spawn_many(N, [], Mod, Func, Args).

send_many([], _) ->
	true;
send_many(Pids, Msg) ->
	[Pid|Res] = Pids,
	Pid ! Msg,
	send_many(Res, Msg).

test(ProcessCount, N) ->
	Busy_Pid = spawn(?MODULE, busy_worker, []),
	Pids = spawn_many(ProcessCount, ?MODULE, worker, [Busy_Pid]),
	L = reverse:start(N),
	send_many(Pids, stop),
	Busy_Pid ! stop,
	L.
