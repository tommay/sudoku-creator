-module(limiter).
-export([start/2, run/2, run/3]).

-define(RUN, run).
-define(RESULT, result).

%% This code uses a semaphore to limit the number of spawned
%% processes.  Spawned processes can also be limited by using a
%% process pool (see pool.erl), but this is somewhat faster.  And the
%% code is simpler.


%% Starts a semaphore process and registers under the given name.
%%
start(Name, Permits) ->
    Pid = spawn(fun () -> loop(Name, Permits) end),
    register(Name, Pid).

%% Spawns a process to run Func if there is a permit available, else
%% runs Func in-process.
%%
run(Name, Func) ->
    Name ! {self(), ?RUN, Func},
    return_result(Name, Func).

run(Name, Func, LinkTo) ->
    Name ! {self(), ?RUN, {Func, LinkTo}},
    return_result(Name, Func).

return_result(Name, Func) ->
    receive
	{Name, ?RESULT, true} ->
	    ok;
	{Name, ?RESULT, false} ->
	    Func()
    end.

loop(Name, Permits) ->
    receive
	{Pid, ?RUN, Msg} ->
	    case Permits == 0 of
		true ->
		    Pid ! {Name, ?RESULT, false},
		    loop(Name, Permits);
		false ->
		    Pid ! {Name, ?RESULT, true},
		    run(Msg),
		    loop(Name, Permits - 1)
	    end;
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    loop(Name, Permits + 1)
    end.

run(Func) when is_function(Func) ->
    monitor(process, spawn(fun () -> Func() end));
run({Func, LinkTo}) when is_function(Func) ->
    monitor(process, spawn(fun () -> link_and_call(LinkTo, Func) end)).

link_and_call(LinkTo, Func) ->
    %% The LinkTo process may already have exited in which case we
    %% shouldn't run.  So catch that error and return without calling
    %% Func.
    try link(LinkTo) of
	true ->
	    Func()
    catch
	error: noproc ->
	    ok
    end.
