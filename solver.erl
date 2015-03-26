-module(solver).
-include("puzzle.hrl").
-export([spawn_solver/2, receive_solutions/1]).

%% Spawns a process to try to solve the Puzzle then report back solved
%% or failed to the Collector, and possibly spawns further processes
%% that also report back to the Collector.  Sends the Collector a
%% started message for its bookkeeping.
%%
spawn_solver(This, Collector) when ?is_puzzle(This), is_pid(Collector) ->
    %% Need to send "started" from the current process so the
    %% Collector receives it before it receives our failed
    %% message, adjusts its count, and possibly finishes.
    Collector ! started,
    limiter:run(limiter, fun () -> solve(This, Collector) end).

%% Try to solve this Puzzle then report back solved or failed to the
%% Collector, and possibly spawns further processes that also report
%% back to the Collector.
%%
solve(This, Collector) when ?is_puzzle(This), is_pid(Collector) ->
    %% We get here either because we're done, we've failed, or we have
    %% to guess and recurse.  We can distinguish by examining the
    %% unplaced position with the fewest possibilities remaining.

    MinPosition = positions:min_by_possible_size(This#puzzle.positions),
    Possible = position:get_possible(MinPosition),

    case Possible == undefined of
	true ->
            %% Solved.  Return This as a solution.
	    Collector ! {solved, This};
	false ->
	    case possible:size(Possible) of
		0 ->
		    %% Failed.  Return no solutions.
		    Collector ! failed;
		_ ->
		    %% Found an unplaced position with two or more
		    %% possibilities.  Guess each possibility and
		    %% either spawn a solver or (for the last
		    %% possibility) recurse.
		    PossibileDigitList = possible:to_list(Possible),
		    do_guesses(This, Collector,
			       position:get_number(MinPosition),
			       PossibileDigitList)
	    end
    end.

%% If this is the last guess then just recurse in thie process.  If
%% there are more guesses to make then spawn a solver for this one.
%%
do_guesses(This, Collector, Number, [Digit]) ->
    Guess = puzzle:place(This, Number, Digit),
    solve(Guess, Collector);
do_guesses(This, Collector, Number, [Digit|Rest]) ->
    Guess = puzzle:place(This, Number, Digit),
    spawn_solver(Guess, Collector),
    do_guesses(This, Collector, Number, Rest).

%% Keep track of pending results and accumulate the solutions we've
%% gotten so far, and recurse until there are no pending results left.
%%
receive_solutions(Yield) ->
    receive_solutions(0, Yield).

receive_solutions(Pending, Yield) ->
    receive
	started ->
	    stats:spawned(),
	    receive_solutions(Pending + 1, Yield);
	{solved, Puzzle} ->
	    stats:solved(),
	    Yield(Puzzle),
	    maybe_receive_solutions(Pending - 1, Yield);
	failed ->
	    stats:failed(),
	    maybe_receive_solutions(Pending - 1, Yield);
	Msg ->
	    io:format("wtf: ~p~n", [Msg]),
	    receive_solutions(Pending, Yield)
    end.

maybe_receive_solutions(0, _Yield) ->
    ok;
maybe_receive_solutions(Pending, Yield) ->
    receive_solutions(Pending, Yield).
