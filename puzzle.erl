-module(puzzle).
-include("puzzle.hrl").
-export([new/0, new/1, position_with_fewest_possibilities/1,
	 random_unplaced_position/1, symmetric_position/2,
	 place/3, foreach_solution/2, to_puzzle/1, print_puzzle/1]).

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

%%
random_unplaced_position(This) ->
    positions:random_unplaced_position(This#puzzle.positions).

%% Returns the Position wihch is symmetric across the center of the board.
%% Used to build symmetric puzzles.  Returns none for the center Position.
%%
symmetric_position(This, Position) when ?is_puzzle(This) ->
    positions:symmetric_position(This#puzzle.positions, Position).

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

%% Solve Puzzles and call Yield with each solved Puzzle.
%%
foreach_solution(This, Yield) when ?is_puzzle(This), is_function(Yield) ->
    solver:spawn_solver(This, self()),
    solver:receive_solutions(Yield).

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
