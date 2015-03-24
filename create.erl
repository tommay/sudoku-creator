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
    case solution_count(Puzzle) of
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

%% Returns the number of solutions the Puzzle has: 0, 1, or more.
%% This could be parallelized.  As soon as we get 2 solutions we're
%% done.  All the Solver processes link to Collector so they die when
%% Collector dies.  Counter runs in a separate process so it can die
%% and be cleaned up along with its mailbox once we know the answer.
%% Counter and Collector are linked so when Counter does
%% exit(intentional_crash) Collector (and everything else) die.
%%
solution_count(Puzzle) ->
    Main = self(),

    %% Self/Main waits for a message from Counter.

    Counter = spawn(fun () ->
			    Main ! count_solutions(0),
			    exit(intentional_crash)
		    end),

    %% Counter waits for messages from Collector/Yield.

    Yield = fun (_Puzzle) -> Counter ! solved end,
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

    receive N -> N end.

%% Only bother counting to 2.
count_solutions(2) ->
    2;
count_solutions(N) ->
    receive
	solved ->
	    count_solutions(N + 1);
	done ->
	    N
    end.
