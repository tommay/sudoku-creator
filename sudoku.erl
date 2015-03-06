-module(sudoku).
-export([start/1]).
-export([s/0]).

s() ->
    start("../evil1.txt").

%% Initializes Puzzle from the given Filename and prints out solutions
%% if any.
%%
start(Filename) ->
    stats:start(),
    semaphore:start(limiter, 20),
    Setup = get_setup(Filename),
    Puzzle = puzzle:new(Setup),
    puzzle:foreach_solution(
      Puzzle,
      fun (SolvedPuzzle) ->
	      io:format("~n"),
	      puzzle:print_puzzle(SolvedPuzzle),
	      io:format("~n")
      end),
%%    io:format("~w solutions~n", [length(Solutions)]),
    io:format("stats: ~s~n", [stats:to_string(stats:get())]).

get_setup(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    NoComments = re:replace(Raw, "#.*", "", [global, {return, list}]),
    re:replace(NoComments, "\\s+", "", [global, {return, list}]).
