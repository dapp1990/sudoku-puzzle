:-lib(ic).
:- compile("sudex_toledo").

sudoku(Ipuzzle):- 
  puzzles(Puzzle, Ipuzzle),
  writeln(IdPuzzle),
  statistics(runtime, _),
  convertBoard(Puzzle,Board),
  solve(Board),
  statistics(runtime, [_, Time]),
  write(Time), 
  writeln('ms'),
  print_solution(Board),
  fail.


solve(BoardVP2) :-
  alldifferent(BoardVP2),

  ( multifor([J,I],1,9,1),
    param(BoardVP2)
    do
      (I-1)*9 #< BoardVP2[J,I],
      (I-1)*9 + 9 #>= BoardVP2[J,I]
  ),
  ( for(I,1,9), 
    param(BoardVP2)
    do
      N1 #= BoardVP2[I,1],
         N2 #= BoardVP2[I,2] - 9,
         N3 #= BoardVP2[I,3] - 18,
         N4 #= BoardVP2[I,4] - 27,
         N5 #= BoardVP2[I,5] - 36,
         N6 #= BoardVP2[I,6] - 45,
         N7 #= BoardVP2[I,7] - 54,
         N8 #= BoardVP2[I,8] - 63,
         N9 #= BoardVP2[I,9] - 72,
      
      alldifferent([N1,N2,N3,N4,N5,N6,N7,N8,N9])
  ),

/**
* different in each square
**/

( multifor([I,J],[1,1],[9,7],[1,3]),
    param(BoardVP2)
    do
      N1 #= BoardVP2[I,J] - (J-1)*9,
      N2 #= BoardVP2[I,J+1] - (J)*9,
      N3 #= BoardVP2[I,J+2] - (J+1)*9,
      differentSquare(N1,N2,N3)
  ),

  search(BoardVP2).

differentSquare(N1, N2,N3) :- 
  (
    N1 #> 0 and 
    N1 #< 4 and
    N2 #>3 and
    N2 #< 7 and
    N3 #>6 and
    N3 #< 10
  )
  or
  (
    N1 #> 0 and
    N1 #< 4 and
    N3 #>3 and
    N3 #< 7 and
    N2 #>6 and
    N2 #< 10
  )
  or
  (
    N2 #> 0 and
    N2 #< 4 and
    N1 #>3 and
    N1 #< 7 and
    N3 #>6 and
    N3 #< 10
  )
  or
  (
    N2 #> 0 and
    N2 #< 4 and
    N3 #>3 and
    N3 #< 7 and
    N1 #>6 and
    N1 #< 10
  )
  or
  (
    N3 #> 0 and
    N3 #< 4 and
    N1 #>3 and
    N1 #< 7 and
    N2 #>6 and
    N2 #< 10
  )
  or
  (
    N3 #> 0 and
    N3 #< 4 and
    N2 #>3 and
    N2 #< 7 and
    N1 #>6 and
    N1 #< 10
  ).

print_solution(Board) :-
  dim(Board2,[9,9]),
  Board2 :: 1..9,
  ( multifor([Number,I],1,9,1),
    param(Board, Board2)
    do
      (I-1)*9 + J #= Board[Number,I],
      Number is Board2[I,J]
  ),
  
  ( foreacharg(El,Board2)
    do
      writeln(El)
  ).

convertBoard(Puzzle, Board) :-
  convert(Puzzle,ArrayPuzzle),

  dim(ArrayPuzzle,[9,9]), 
  ArrayPuzzle[1..9,1..9] :: [1..9], 

  dim(Board,[9,9]),
  Board[1..9,1..9] :: [1..81],

  convertSecondApproach(ArrayPuzzle,Board).

convert(ListOfList, Board) :-
  ( foreach(List, ListOfList), foreach(Array, ListOfArray)
   do
      array_list(Array, List)
  ),
  array_list(Board,ListOfArray).

convertSecondApproach(ArrayPuzzle,Board) :-
  (multifor([I,J],1,9,1),
    param(ArrayPuzzle,Board)
    do
    Number is ArrayPuzzle[I,J],
    (is_solver_var(Number) ->
       true
       ;
       (I-1)*9 + J #= Board[Number,I]
    )
  ).

search(Board) :- 
  term_variables(Board, Vars), 

  %once search(Vars,0,input_order, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,anti_first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,smallest, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,largest, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,occurrence, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,most_constrained, indomain, complete, [backtrack(A)]),

  %%once search(Vars,0,smallest, indomain_min, complete, [backtrack(A)]),
  %%once search(Vars,0,smallest, indomain_middle, complete, [backtrack(A)]),
  once search(Vars,0,smallest, indomain_max, complete, [backtrack(A)]),

  writeln(backtracks - A).