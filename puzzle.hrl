-record(puzzle, {positions, exclusions}).
-define(is_puzzle(Term), is_record(Term, puzzle)).
