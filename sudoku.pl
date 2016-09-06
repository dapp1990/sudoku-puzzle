:- lib(ic).
:- compile("sudex_toledo").

sudoku(IdPuzzle) :- 
  writeln(' '), 
  puzzles(Puzzle, IdPuzzle),
  writeln(IdPuzzle), 
  statistics(runtime, _),
  convert(Puzzle, Board),
  solve(Board),
  statistics(runtime, [_, Time]),
  write(Time), 
  writeln('ms'),
  print_solution(Board),
  fail.

convert(ListOfList, Board) :-
  ( foreach(List, ListOfList), foreach(Array, ListOfArray)
   do
      array_list(Array, List)
  ),
  array_list(Board,ListOfArray).

solve(Board) :- 
  constraints(Board),
  search(Board).

constraints(Board) :- 
  dim(Board,[9,9]), 
  Board[1..9,1..9] :: [1..9], 
  ( for(I,1,9), 
    param(Board) 
    do
        Row is Board[I,1..9],
        alldifferent(Row),
        Col is Board[1..9,I],
        alldifferent(Col)
  ),
  (multifor([I,J],1,9,3),
    param(Board)
    do
      S is Board[I..I+2,J..J+2],
      flatten(S, SubSquare),
      alldifferent(SubSquare)
  ).

search(Board) :- 
  term_variables(Board, Vars), 
  %%once search(Vars,0,input_order, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,anti_first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,smallest, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,occurrence, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,most_constrained, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,largest, indomain, complete, [backtrack(A)]),

  %%once search(Vars,0,first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,first_fail, indomain_min, complete, [backtrack(A)]),
  %%once search(Vars,0,first_fail, indomain_max, complete, [backtrack(A)]),

  once search(Vars,0,first_fail, indomain_middle, complete, [backtrack(A)]),

  writeln(backtracks - A).

print_solution(Board) :-
  ( foreacharg(El,Board)
    do
      writeln(El)
  ).