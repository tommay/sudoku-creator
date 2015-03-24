-module(possible).
-export([new/0, remove/2, size/1, to_list/1]).
-export([to_string/1]).

-record(possible, {list}).
-define(is_possible(Term), is_record(Term, possible)).

%% An object that maintains the possible digits for a position.

new() ->
    List = spud:sort_by(
	     lists:seq(1, 9),
	     fun (_E) -> rnd:uniform() end),
    #possible{list = List}.

remove(This, Digit) ->
    This#possible{list = lists:delete(Digit, This#possible.list)}.

size(This) ->
    length(This#possible.list).

to_list(This) ->
    This#possible.list.

to_string(This) ->
    spud:format("~w", [to_list(This)]).

