-module(unknown).
-export([new/1, cell_number/1, possible/1, min_by_num_possible/1, place/3]).

-include("unknown.hrl").

%% Returns a new unknown for CellNumber.  Determine the
%% unknown's row, column, and square, set all digits possible.
%%
new(CellNumber) when is_number(CellNumber)->
  Row = CellNumber div 9,
  Col = CellNumber rem 9,
  Square = (Row div 3)*3 + (Col div 3),
  #unknown{
     cell_number = CellNumber,
     row = Row,
     col = Col,
     square = Square,
     possible = lists:seq(1, 9)
    }.

cell_number(This)
  when ?is_unknown(This) ->
    This#unknown.cell_number.

possible(This)
  when ?is_unknown(This) ->
    This#unknown.possible.

place(This, Other, Digit)
  when ?is_unknown(This), ?is_unknown(Other), is_number(Digit) ->
    case is_excluded_by(This, Other) of
	true ->
	    This#unknown{possible =
			     lists:delete(Digit, This#unknown.possible)};
	false ->
	    This
    end.

%% Returns true if this and Other are in the same row, column, or
%% square, else false.
%% An Unknown does not exclude itself.  I'm not sure we actually
%% have to check for this in practice, but better safe than sorry.
%%
is_excluded_by(This, Other)
  when ?is_unknown(This), ?is_unknown(Other) ->
    (This#unknown.cell_number /= Other#unknown.cell_number) andalso
	((This#unknown.row == Other#unknown.row) orelse
         (This#unknown.col == Other#unknown.col) orelse
	 (This#unknown.square == Other#unknown.square)).

min_by_num_possible(List) ->
  min_by(fun (X) -> length(X#unknown.possible) end, List).

min_by(Func, [H|T]) when is_function(Func)->
  min_by(Func, Func(H), H, T).

min_by(_Func, _Min, Elem, [])
  when is_function(_Func) ->
    Elem;
min_by(Func, Min, Elem, [H|T])
  when is_function(Func), is_number(Min) ->
    N = Func(H),
    case N < Min of
	true ->
	    min_by(Func, N, H, T);
	false ->
	    min_by(Func, Min, Elem, T)
    end.
