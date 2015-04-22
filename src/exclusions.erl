-module(exclusions).
-export([new/0, get_list_for_position/2]).

%% Returns an array mapping Number to a list of all positions excluded
%% by that position number, e.g., 0 => [0, 1, 2, ...].

new() ->
    ExclusionLists = create_exclusion_lists(),
    ByPosition =
	[get_exclusions_for_position(ExclusionLists, Number)
	 || Number <- lists:seq(0, 80)],
    array:from_list(ByPosition).

%% Returns a list for each row, column, and square listing the
%% positions it contains: [[0, 1, 2, ...], [0, 9, 18, ...], ...]
%%
create_exclusion_lists() ->
    ZeroToEight = lists:seq(0, 8),

    %% Create a List for each row, containing the Number of each
    %% position in the row.
    %% XXX USe Exclusion objects with names?

    Rows = [[Row*9 + Col || Col <- ZeroToEight] || Row <- ZeroToEight],

    %% Same for columns.

    Cols = [[Row*9 + Col || Row <- ZeroToEight] || Col <- ZeroToEight],

    %% Same for squared.

    Squares =
	lists:map(
		fun (Square) ->
		    %% Row and Col of upper left corner of square.
		    Row = Square div 3 * 3,
		    Col = Square rem 3 * 3,
		    [(Row + N div 3)*9 + (Col + N rem 3) || N <- ZeroToEight]
		end,
		ZeroToEight),

    Rows ++ Cols ++ Squares.

%% Used during initialization.
%%
get_exclusions_for_position(ExclusionLists, Number)
  when is_list(ExclusionLists), is_number(Number) ->
    ByPosition =
	[ExclusionList || ExclusionList <- ExclusionLists,
			  lists:member(Number, ExclusionList)],
    Flattened = lists:flatten(ByPosition),
    Filtered = [N || N <- Flattened, N /= Number],
    spud:uniq(Filtered).

%% Used by place.
%%
get_list_for_position(This, Number) when is_number(Number) ->
    array:get(Number, This).

