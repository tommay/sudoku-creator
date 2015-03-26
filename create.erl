-module(create).
-export([start/0]).

start() ->
    limiter:start(limiter, 50),
    stats:start(),
    stuff:start(),
    rnd:start(),
    Puzzle = puzzle:new(),
    create_from(Puzzle).

create_from(Puzzle) ->
    Position = puzzle:random_unplaced_position(Puzzle),
    SymmetricPosition = puzzle:symmetric_position(Puzzle, Position),
    case SymmetricPosition of
	none ->
	    create_from(Puzzle, Position);
	_ ->
	    create_from(Puzzle, Position, SymmetricPosition)
    end.

create_from(Puzzle, Position) ->
    Digits = possible:to_list(position:get_possible(Position)),
    spud:map_find(
      Digits,
      fun (D) ->
	      Puzzle2 = puzzle:place(
			  Puzzle, position:get_number(Position), D),
	      check(Puzzle2)
      end).

create_from(Puzzle, Position, SymmetricPosition) ->
    Digits =
	spud:product(
	  possible:to_list(position:get_possible(Position)),
	  possible:to_list(position:get_possible(SymmetricPosition))),
    spud:map_find(
      Digits,
      fun ({A, B}) ->
	      Puzzle2 = puzzle:place(Puzzle, position:get_number(Position), A),
	      Puzzle3 = puzzle:place(Puzzle2, position:get_number(SymmetricPosition), B),
	      check(Puzzle3)
      end).

check(Puzzle) ->
    io:format("~p Checking:~n~s~n", [self(), puzzle:to_puzzle(Puzzle)]),
    case length(get_solutions(Puzzle, 2)) of
	0 ->
	    io:format("~p No solutions.~n", [self()]),
	    false;				% Keep looking.
	1 ->
	    io:format("~p One solution!~n", [self()]),
	    Puzzle;				% Created a puzzle.
	_ ->
	    io:format("~p Multiple solutions . . .~n", [self()]),
	    create_from(Puzzle)			% Look deeper.
    end.

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
