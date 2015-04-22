-module(stats).
-behavior(gen_server).
-export([start/0, solved/0, failed/0, guess/0, reset/0,
	 get/0, get_failed/0, to_string/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-compile({no_auto_import,[get/0]}).

-record(stats, {solved = 0, failed = 0, guess = 0}).

-define(cast(Msg),
	Msg() ->
	       gen_server:cast(stats, Msg)).
-define(handle(Msg),
	handle_cast(Msg, State) ->
	       {noreply, ?increment(State, Msg)}).
-define(increment(Rec, Field),
	 Rec#stats{Field = Rec#stats.Field + 1}).

start() ->
    gen_server:start({local, stats}, ?MODULE, [], []).

init(_Args) ->
    {ok, #stats{}}.

?cast(solved).
?cast(failed).
?cast(guess).
?cast(reset).

get() ->
    gen_server:call(stats, get).

get_failed() ->
    gen_server:call(stats, get_failed).


?handle(solved);
?handle(failed);
?handle(guess);
handle_cast(reset, _State) ->
    {noreply, #stats{}}.

handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(get_failed, _From, State) ->
    {reply, State#stats.failed, State}.

to_string(Stats) ->
    spud:format("solved: ~w guess: ~w failed: ~w",
		[Stats#stats.solved, Stats#stats.guess, Stats#stats.failed]).
