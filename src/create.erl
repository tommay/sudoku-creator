-module(create).
-export([main/0]).

style() ->
    spinny.

main() ->
    application:start(stats),
    application:start(rnd),
    Puzzle = create_with_no_guessing(layout:layout(style())),
    io:format("~s~n", [puzzle:to_puzzle_string(Puzzle)]).

create_with_no_guessing(Layout) ->
    Puzzle = create(Layout),
    stats:reset(),
    puzzle:solve(Puzzle),
    io:format("~s~n", [stats:to_string(stats:get())]),
    case stats:get_guess() == 0 of
	true ->
	    Puzzle;
	false ->
	    io:format("~s~n", [puzzle:to_puzzle_string(Puzzle)]),
	    create_with_no_guessing(Layout)
    end.

create(Layout) ->
    %% Randomly eliminate cells from a random solved puzzle using the
    %% given layout and making sure the puzzle remains solveable with
    %% a single solution.
    lists:foldl(
      fun (CellNumbers, Puzzle) ->
	      NewPuzzle = puzzle:remove(Puzzle, CellNumbers),
	      Solutions = puzzle:solve(NewPuzzle, 2),
	      case length(Solutions) == 1 of
		  true -> NewPuzzle;
		  false -> Puzzle
	      end
      end,
      puzzle:create_random_puzzle(),
      util:shuffle(Layout)).
