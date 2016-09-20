-module(puzzle).
-export([create_random_puzzle/0, solve/1, solve/2, remove/2]).
-export([to_puzzle_string/1]).

-include("puzzle.hrl").
-include("unknown.hrl").

%% Returns a new Puzzle with empty Cells.
%%
new() ->
    #puzzle{
       placed = [],
       unknown = [unknown:new(N) || N <- lists:seq(0, 80)]
      }.

create_random_puzzle() ->
    solve(new()).

%% Try to solve this Puzzle and return a single solution.
%%
solve(This) ->
    [Solution] = solve(This, 1),
    Solution.

%% Try to solve this Puzzle and return up to Remaining possible solutions.
%%
solve(This, Remaining) when ?is_puzzle(This), is_number(Remaining) ->
    {Solutions, _Remaining} = solve(This, {[], Remaining}),
    Solutions;
solve(_This, Result = {_Solutions, _Remaining = 0}) when ?is_puzzle(_This) ->
    Result;
solve(This, Result = {Solutions, Remaining}) when ?is_puzzle(This) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    case This#puzzle.unknown of
	[] ->
            %% Solved.  Return This as a solution.
	    stats:solved(),
	    {[This | Solutions], Remaining - 1};
	Unknowns ->
	    MinUnknown = unknown:min_by_num_possible(Unknowns),
	    case unknown:possible(MinUnknown) of
		[] ->
		    %% Failed.  Return what we got so far.
		    stats:failed(),
		    Result;
		Possible ->
		    %% Found an unknown cell with one or more
		    %% possibilities.  Guess each possibility and
		    %% recurse while Remaining != 0.
		    case Possible of
			[_] -> undefined;
			_ -> stats:guess()
		    end,
		    do_guesses(This, Result, MinUnknown, util:shuffle(Possible))
	    end
    end.

do_guesses(_This, Result, _Unknown, []) ->
    Result;
do_guesses(This, Result = {_Solutions, _Remaining}, Unknown, [Digit|Rest]) ->
    Guess = place(This, Unknown, Digit),
    Result2 = {_, Remaining2} = solve(Guess, Result),
    case Remaining2 == 0 of
	true ->
	    Result2;
	false ->
	    do_guesses(This, Result2, Unknown, Rest)
    end.

%% Returns a new Puzzle with Digit placed in Cell CellNumber.  The
%% possible sets of all Cells are updated to account for the new
%% placement.
%%
place(This, CellNumber, Digit)
  when is_number(CellNumber) ->
    place(This, unknown:new(CellNumber), Digit);
place(This, Unknown, Digit)
  when ?is_puzzle(This), ?is_unknown(Unknown), is_number(Digit) ->
    CellNumber = unknown:cell_number(Unknown),
    Placed = [{CellNumber, Digit} | This#puzzle.placed],
    Unknown2 = lists:filtermap(
		 fun (E) ->
			 case unknown:cell_number(E) /= CellNumber of
			     true -> {true, unknown:place(E, Unknown, Digit)};
			     false -> false
			 end
		 end,
		 This#puzzle.unknown),
    This#puzzle{placed = Placed, unknown = Unknown2}.

remove(This, CellNumbers) when ?is_puzzle(This) ->
    lists:foldl(
      fun ({Number, Digit}, Puzzle) ->
	      case lists:member(Number, CellNumbers) of
		  true ->
		      Puzzle;
		  false ->
		      place(Puzzle, Number, Digit)
	      end
      end,
      new(),
      This#puzzle.placed).

%% Returns a raw string of 81 digits and dashes, like the argument to new.
%%
to_string(This) when ?is_puzzle(This) ->
    Placed = [{Number, $0 + Digit} || {Number, Digit} <- This#puzzle.placed],
    Unknown = [{unknown:cell_number(U), $-} || U <- This#puzzle.unknown],
    All = Placed ++ Unknown,
    Sorted = lists:sort(All),
    [Char || {_, Char} <- Sorted].

%% Returns a string that prints out as a grid of digits.
%%
to_puzzle_string(This) when ?is_puzzle(This) ->
    String = to_string(This),
    string:join(
      lists:map(
	fun (Rows) ->
		string:join(
		  lists:map(
		    fun (Row) ->
			    string:join(spud:slices(Row, 3), " ")
		    end,
		    spud:slices(Rows, 9)),
		  "\n")
	end,
	spud:slices(String, 27)),
      "\n\n").
