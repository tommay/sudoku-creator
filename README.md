sudoku creator
==============

More fun with Erlang.  This time I'm creating sudoku puzzles.

The creator works by creating a random solution then removing random
digits in the classic symmetric manner while checking that there is
still only one possible solution.

The creator uses a simplified (non-concurrent) version of my sudoku solver
to:
* Create a solution by randomly finding a solution to an empty Puzzle.
* Check that potential puzzles have exactly one solution.

The history has a version of the solver that uses linked processes so
the solver can be easily shut down after enough solutions are found,
but since at most two solutions ever need to be found I backed off and
just do everything in-process and stop when enough solutions have been
found.

This is kind of fun because it can create puzzles with different
symmetry than what you usually see.

There is no attempt to grade the difficulty level of the puzzles.  As
a simple heuristic, only puzzles that the solver can handle without
guessing are returned.

    $ git clone ...
    $ cd sudoku-creator
    $ make
    $ ./create
