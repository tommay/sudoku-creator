-module(layout).
-export([layout/1]).

layout(Style) ->    
    Func =
	case Style of
	    classic -> fun classic/1;
	    spinny -> fun spinny/1
	end,
    Layout = [uniq(Func(N)) || N <- lists:seq(0, 80)],
    uniq_by(fun (E) -> lists:min(E) end, Layout).

classic(N) ->
    [N, 80 - N].

spinny(N) ->
    spinny(N, []).

spinny(N, Result) ->
    case lists:member(N, Result) of
	true -> Result;
	false ->
	    {Row, Col} = rowcol(N),
	    Row2 = Col,
	    Col2 = 8 - Row,
	    spinny(n(Row2, Col2), [N | Result])
    end.

uniq(List) ->
    uniq_by(fun (E) -> E end, List).

uniq_by(Func, List) ->
    Map = maps:from_list([{Func(E), E} || E <- List]),
    maps:values(Map).

rowcol(N) ->
    {N div 9, N rem 9}.

n(Row, Col) ->
    Row*9 + Col.
