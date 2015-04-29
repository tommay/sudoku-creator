-module(stats).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile({no_auto_import,[get/0]}).

-record(stats, {solved = 0, failed = 0, guess = 0}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([solved/0, failed/0, guess/0, reset/0,
	 get/0, get_failed/0]).
-export([to_string/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-define(cast(Msg),
	Msg() ->
	       gen_server:cast(stats, Msg)).

?cast(solved).
?cast(failed).
?cast(guess).
?cast(reset).

get() ->
    gen_server:call(stats, get).

get_failed() ->
    gen_server:call(stats, get_failed).

to_string(Stats) ->
    spud:format("solved: ~w guess: ~w failed: ~w",
		[Stats#stats.solved, Stats#stats.guess, Stats#stats.failed]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #stats{}}.

handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(get_failed, _From, State) ->
    {reply, State#stats.failed, State}.

-define(handle(Msg),
	handle_cast(Msg, State) ->
	       {noreply, ?increment(State, Msg)}).
-define(increment(Rec, Field),
	 Rec#stats{Field = Rec#stats.Field + 1}).

?handle(solved);
?handle(failed);
?handle(guess);
handle_cast(reset, _State) ->
    {noreply, #stats{}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
