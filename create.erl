-module(create).
-export([start/0]).

start() ->
    limiter:start(limiter, 50),
    stats:start(),
    stuff:start(),
    rnd:start(),
    Puzzle = create(),
    puzzle:print_puzzle(Puzzle).

create() ->
    %% Create a solved Puzzle by getting a random solution to an empty Puzzle.
    [Solved | _] = get_solutions(puzzle:new(), 1),
    puzzle:print_puzzle(Solved),
    io:format("stats: ~s~n", [stats:to_string(stats:get())]),
    SolvedString = puzzle:to_string(Solved),
    %% Randomly eliminate symmetric positions and make sure the puzzle
    %% remains solvable with a single solution.  Not so awesome.
    Numbers = spud:sort_by(
		lists:seq(1, 41),
		fun (_) -> rnd:uniform() end),
    PuzzleString = 
	lists:foldl(
	  fun (Number, String) ->
		  NewString = eliminate_symmetric(String, Number),
		  Puzzle = puzzle:new(NewString),
		  case length(get_solutions(Puzzle, 2)) of
		      0 -> String;
		      1 -> NewString;
		      2 -> String
		  end
	  end,
	  SolvedString,
	  Numbers),
    puzzle:new(PuzzleString).

eliminate_symmetric(String, 41) ->
    eliminate(String, 41);
eliminate_symmetric(String, N) ->
    eliminate(
      eliminate(String, N),
      82 - N).

eliminate(String, N) ->
    tuple_to_list(setelement(N, list_to_tuple(String), $-)).

%% Returns solutions of Puzzle, up to Max number of solutions.  If Max
%% is 1, we can tell whether Puzzle has a solution, and get a
%% solution.  If Max is 2 we can tell whether Puzzle has multiple
%% solutions.  As soon as we get Max solutions we're done; Counter
%% messages us (Main) the result and exits abnormally which takes down
%% Collector which takes down all the Solvers linked to it.
%%
get_solutions(Puzzle, Max) ->
    Main = self(),

    %% Self/Main waits for a message from Counter.

    Counter = spawn(fun () ->
			    Main ! get_solutions_loop(Max, []),
			    exit(done)
		    end),

    %% Counter waits for messages from Collector/Yield.

    Yield = fun (Solution) -> Counter ! {solved, Solution} end,
    Collector = spawn(fun () ->
			      %% Kill the Collector (and Solvers) when
			      %% Counter exits.
			      link(Counter),
			      solver:receive_solutions(Yield),
			      Counter ! done
		      end),

    %% Collector waits for messages from the Solvers.

    solver:spawn_solver(Puzzle, Collector),

    %% Self/Main waits here.

    receive Solutions -> Solutions end.

get_solutions_loop(0, Solutions) ->
    Solutions;
get_solutions_loop(Remaining, Solutions) ->
    receive
	{solved, Puzzle} ->
	    get_solutions_loop(Remaining - 1, [Puzzle | Solutions]);
	done ->
	    Solutions
    end.
