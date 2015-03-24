-module(rnd).
-export([start/0, start/1, uniform/0, uniform/1]).

%% A random number server.

start() ->
    start(now()).

start(Seed) ->
    Pid = spawn(fun () -> random:seed(Seed), loop() end),
    register(rnd, Pid).

uniform() ->
    rnd ! {self(), uniform},
    receive_value().

uniform(N) ->
    rnd ! {self(), uniform, N},
    receive_value().

receive_value() ->
    receive
	{rnd, Value} ->
	    Value
    end.

loop() ->			
    receive
	{From, uniform} ->
	    From ! {rnd, random:uniform()},
	    loop();
	{From, uniform, N} ->
	    From ! {rnd, random:uniform(N)},
	    loop()
    end.
