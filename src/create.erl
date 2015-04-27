-module(create).
-export([main/0]).

main() ->
    stats:start_link(),
    rnd:start_link(),
    create_with_no_guessing().

create_with_no_guessing() ->
    %% Create a solved Puzzle by getting a random solution to an empty Puzzle.
    [Solved | _] = puzzle:solve(puzzle:new(), 1),
    puzzle:print_puzzle(Solved),
    io:format("stats: ~s~n", [stats:to_string(stats:get())]),
    create_with_no_guessing(Solved).

create_with_no_guessing(Solved) ->
    Puzzle = create_from(Solved),
    puzzle:print_puzzle(Puzzle),
    stats:reset(),
    puzzle:solve(Puzzle, 1000000),
    io:format("stats: ~s~n", [stats:to_string(stats:get())]),
    case stats:get_failed() == 0 of
	true ->
	    ok;
	false ->
	    create_with_no_guessing(Solved)
    end.

create_from(Solved) ->
    SolvedString = puzzle:to_string(Solved),
    %% Randomly eliminate symmetric positions and make sure the puzzle
    %% remains solvable with a single solution.  Not so awesome.
    Numbers = spud:sort_by(
		lists:seq(1, 81),
		fun (_) -> rnd:uniform() end),
    PuzzleString = 
	lists:foldl(
	  fun (Number, String) ->
		  NewString = eliminate_symmetric(String, Number),
		  Puzzle = puzzle:new(NewString),
		  case length(puzzle:solve(Puzzle, 2)) of
		      0 -> String;
		      1 -> NewString;
		      2 -> String
		  end
	  end,
	  SolvedString,
	  Numbers),
    puzzle:new(PuzzleString).

eliminate_symmetric(String, N) ->
    case (N - 1) rem 9 of
	R when R < 4 ->
	    eliminate(
	      eliminate(String, N),
	      N + (8 - R*2));
	4 ->
	    eliminate(String, N);
	_ ->
	    String
    end.

eliminate(String, N) ->
    tuple_to_list(setelement(N, list_to_tuple(String), $-)).
