-module(stats).
-export([start/0, solved/0, failed/0, guess/0, reset/0, get/0,
	 get_failed/0, to_string/1]).
-compile({no_auto_import,[get/0]}).

-record(stats, {solved = 0, failed = 0, guess = 0}).

-define(increment(Rec, Field), Rec#stats{Field = Rec#stats.Field + 1}).
-define(send(Type),
	Type() ->
	       stats ! Type).

start() ->
    Pid = spawn(fun () -> loop(#stats{}) end),
    register(stats, Pid).

?send(solved).
?send(failed).
?send(guess).
?send(reset).

get() ->
    stats ! {self(), get},
    receive
	Stats when is_record(Stats, stats) ->
	    Stats
    end.

loop(This) ->
    receive
	solved ->
	    loop(?increment(This, solved));
	failed ->
	    loop(?increment(This, failed));
	guess ->
	    loop(?increment(This, guess));
	reset ->
	    loop(#stats{});
	{Pid, get} ->
	    Pid ! This,
	    loop(This)
    end.

get_failed() ->
    (get())#stats.failed.

to_string(This) ->
    spud:format("solved: ~w guess: ~w failed: ~w",
		[This#stats.solved, This#stats.guess, This#stats.failed]).
