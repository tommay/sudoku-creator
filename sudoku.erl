-module(sudoku).
-export([start/1]).

start(Filename) ->
    Setup = get_setup(Filename),
    Positions = positions:new(Setup),
    positions:solve(Positions).

get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
