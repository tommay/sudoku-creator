-module(positions).
-export([new/0, update/3, min_by_possible_size/1,
	 random_unplaced_position/1, symmetric_position/2]).
-export([to_string/1]).

%% This allow us to (somewhat) abstract out the data type Puzzle uses
%% to store Positions.

new() ->
    Positions = [position:new(Number) || Number <- lists:seq(0, 80)],
    list_to_tuple(Positions).

update(This, Index, Func) when is_number(Index), is_function(Func) ->
    spud:tuple_update(Index, Func, This).

%% Returns the unplaced Position with the smallest set of
%% possibilities.  This is used to find the best Position to make a
%% guess for to minimize the amount of guessing.
%%
min_by_possible_size(This) ->
    spud:tuple_min_by(
      This,
      fun (Position) ->
	      %% Sort placed positions to the end.
	      case position:get_placed(Position) of
		  undefined ->
		      Possible = position:get_possible(Position),
		      possible:size(Possible);
		  _ ->
		      {10, 0}
	      end
      end).

%% Return a random unplaced position.
%%
random_unplaced_position(This) ->
    Position = spud:sample(This),
    case position:get_placed(Position) of
	undefined ->
	    Position;
	_ ->
	    random_unplaced_position(This)
    end.

%% Returns the Position wihch is symmetric across the center of the board.
%% Used to build symmetric puzzles.  Returns none for the center Position.
%%
symmetric_position(This, Position) ->
    case position:get_number(Position) of
	40 ->
	    none;
	Number ->
	    element(80 - Number + 1, This)
    end.

to_string(This) ->
    lists:map(
      fun (Position) ->
	      case position:get_placed(Position) of
		  undefined ->
		      $-;
		  Digit ->
		      Digit + $0
	      end
      end,
      tuple_to_list(This)).
