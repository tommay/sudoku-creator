-module(puzzle).
-include("puzzle.hrl").
-export([new/0, new/1, solve/2,
	 place/3, to_string/1, to_puzzle/1,
	 print_puzzle/1]).

%% Returns a new Puzzle with empty Positions.
%%
new() ->
    #puzzle{positions = positions:new(),
	    exclusions = exclusions:new()}.

%% Returns a new Puzzle with each Position initialized according to
%% Setup, which is a string of 81 digits or dashes.
%%
new(Setup) ->
    Digits = to_digits(Setup),
    Numbers = lists:seq(0, 80),
    Zipped = lists:zip(Digits, Numbers),
    lists:foldl(
      fun ({Digit, Number}, This) ->
	      case Digit of
		  undefined ->
		      This;
		  _ ->
		      place(This, Number, Digit)
	      end
      end,
      new(),
      Zipped).

%% Given a Setup string, returns a list of numbers or undefined for
%% each position.
%%
to_digits(Setup) ->
    [case Char of
	 $- ->
	     undefined;
	 _ ->
	     Char - $0
     end || Char <- Setup].

%% Returns the position with the fewest possibilities.
%%
position_with_fewest_possibilities(This) when ?is_puzzle(This) ->
    positions:min_by_possible_size(This#puzzle.positions).

%% Returns a new Puzzle with Digit placed at Position AtNumber.  The
%% possible sets of all Positions are updated to account for the new
%% placement.
%%
place(This, AtNumber, Digit)
  when ?is_puzzle(This), is_number(AtNumber), is_number(Digit) ->
    Positions = This#puzzle.positions,
    %% Place the Digit.
    Positions2 = positions:update(
		   Positions,
		   AtNumber,
		   fun (Position) -> position:place(Position, Digit) end),
    %% Exclude Digit from excluded Positions.
    ExclusionList = exclusions:get_list_for_position(
		      This#puzzle.exclusions, AtNumber),
    Positions3 = do_exclusions(Positions2, Digit, ExclusionList),
    This#puzzle{positions = Positions3}.

do_exclusions(Positions, Digit, ExclusionList) ->
    lists:foldl(
      fun (Number, PositionsAccum) ->
	      positions:update(
		PositionsAccum,
		Number,
		fun (Position) ->
			position:not_possible(Position, Digit)
		end)
      end,
      Positions,
      ExclusionList).

%% Try to solve this Puzzle and return up to Remaining possible solutions.
%%
solve(This, 0) when ?is_puzzle(This) ->
    [];
solve(This, Remaining) when ?is_puzzle(This) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = position_with_fewest_possibilities(This),
    Possible = position:get_possible(MinPosition),

    case Possible == undefined of
	true ->
            %% Solved.  Return This as a solution.
	    stats:solved(),
	    [This];
	false ->
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  Return no solutions.
		    stats:failed(),
		    [];
		_ ->
		    %% Found an unplaced position with two or more
		    %% possibilities.  Guess each possibility and
		    %% recurse.
		    stats:guess(),
		    AtNumber = position:get_number(MinPosition),
		    PossibleDigitList = possible:to_list(Possible),
		    blah(This, AtNumber, Remaining, PossibleDigitList)
	    end
    end.

blah(_This, _AtNumber, 0, _PossibleDigitList) ->
    [];
blah(_This, _AtNumber, _Remaining, []) ->
    [];
blah(This, AtNumber, Remaining, [PossibleDigit | T]) ->
    Puzzle2 = puzzle:place(This, AtNumber, PossibleDigit),
    Solutions = solve(Puzzle2, Remaining),
    Solutions ++ blah(This, AtNumber, Remaining - length(Solutions), T).

%% Returns a raw string of 81 digits and dashes, like the argument to new.
%%
to_string(This) when ?is_puzzle(This) ->
    positions:to_string(This#puzzle.positions).

%% Returns a string that prints out as a grid of digits.
%%
to_puzzle(This) when ?is_puzzle(This) ->
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

%% Prints the to_puzzle string.
%%
print_puzzle(This) when ?is_puzzle(This) ->
    io:format("~s~n", [to_puzzle(This)]).
