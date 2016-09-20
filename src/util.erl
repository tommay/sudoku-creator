-module(util).
-export([shuffle/1]).

shuffle(List) ->
    spud:sort_by(
      List,
      fun (_) -> rnd:uniform() end).
