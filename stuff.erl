-module(stuff).
-export([start/0, set/2, get/1]).

%% A global dictionary to store things cross-process for debugging.

start() ->
    Pid = spawn(fun () -> loop(dict:new()) end),
    register(stuff, Pid).

set(Key, Value) ->
    stuff ! {set, Key, Value},
    Value.

get(Key) ->
    Ref = make_ref(),
    stuff ! {get, self(), Ref, Key},
    receive
	{Ref, Value} ->
	    Value
    end.

loop(Dict) ->
    receive
	{set, Key, Value} ->
	    loop(dict:store(Key, Value, Dict));
	{get, From, Ref, Key} ->
	    Result = case dict:find(Key, Dict) of
			 {ok, Value} ->
			     Value;
			 error ->
			     undefined
		     end,
	    From ! {Ref, Result},
	    loop(Dict)
    end.
