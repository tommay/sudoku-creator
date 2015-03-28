-module(stats).
-export([start/0, spawned/0, solved/0, failed/0, reset/0, get/0,
	 get_spawned/0, to_string/1]).
-compile({no_auto_import,[get/0]}).

-record(stats, {spawned = 0, solved = 0, failed = 0, current = 0, max = 0}).

-define(increment(Rec, Field), Rec#stats{Field = Rec#stats.Field + 1}).
-define(send(Type),
	Type() ->
	       stats ! Type).

start() ->
    Pid = spawn(fun () -> loop(#stats{}) end),
    register(stats, Pid).

?send(spawned).
?send(solved).
?send(failed).
?send(reset).

get() ->
    stats ! {self(), get},
    receive
	Stats when is_record(Stats, stats) ->
	    Stats
    end.

track(This, Inc) ->
    Current = This#stats.current + Inc,
    case Current > This#stats.max of
	true -> This#stats{current = Current, max = Current};
	false -> This#stats{current = Current}
    end.

loop(This) ->
    receive
	spawned ->
	    Tracked = track(This, +1),
	    loop(?increment(Tracked, spawned));
	solved ->
	    Tracked = track(This, -1),
	    loop(?increment(Tracked, solved));
	failed ->
	    Tracked = track(This, -1),
	    loop(?increment(Tracked, failed));
	reset ->
	    loop(#stats{});
	{Pid, get} ->
	    Pid ! This,
	    loop(This)
    end.

get_spawned() ->
    (get())#stats.spawned.

to_string(This) ->
    spud:format("spawned: ~w solved: ~w failed: ~w max: ~w",
		[This#stats.spawned, This#stats.solved, This#stats.failed,
		 This#stats.max]).
