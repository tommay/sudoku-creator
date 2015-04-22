-module(rnd).
-behaviour(gen_server).

-export([start/0, uniform/0, uniform/1]).
-export([init/1, handle_call/3]).

start() ->
    gen_server:start({local, rnd}, ?MODULE, [], []).

init(_Args) ->
    {ok, now()}.

uniform() ->
    gen_server:call(rnd, uniform).

uniform(N) ->
    gen_server:call(rnd, {uniform, N}).

handle_call(uniform, _From, State) ->
    {R, State2} = random:uniform_s(State),
    {reply, R, State2};
handle_call({uniform, N}, _From, State) ->
    {R, State2} = random:uniform_s(N, State),
    {reply, R, State2}.
