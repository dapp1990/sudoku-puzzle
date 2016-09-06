:- lib(ic).
:- compile("sudex_toledo").

sudoku(IdPuzzle) :- 

  dim(Board,[9,9]),   
  Board[1..9,1..9] :: [1..9], 

  dim(Board2,[9,9]),
  Board2[1..9,1..9] :: [1..81],

  puzzles(Puzzle, IdPuzzle),
  %writeln(IdPuzzle),
  statistics(runtime, _),
  convert_list_array(Puzzle, Board),
  convert_second_approach(Board, Board2),

  constraints_first_approach(Board),

  constraints_second_approach(Board2),

  channeling_constraints(Board, Board2),

  term_variables(Board, Vars), 
  term_variables(Board2, Vars2), 
  append(Vars, Vars2, Vars3),

  search(Vars3),

  statistics(runtime, [_, Time]),
 
  %write(Time), 
  %writeln('ms'),
  writeln(Time), 
  %print_solution(Board),
  fail.

convert_second_approach(ArrayPuzzle,Board) :-
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


convert_list_array(ListOfList, Board) :-
  ( foreach(List, ListOfList), foreach(Array, ListOfArray)
   do
      array_list(Array, List)
  ),
  array_list(Board,ListOfArray).

/**********************************************
********** Channeling constraints *************
***********************************************/

channeling_constraints(Board1, Board2) :-
 (multifor([R1,C1],1,9,1),
    param(Board1, Board2)
    do
    N1 is Board1[R1,C1],
    Cell is (R1-1)*9 + C1,
    (is_solver_var(N1) ->
       suspend(Cell is Board2[N1,R1],0,[N1]->inst)
       ;
       Cell is Board2[N1,R1]
    )
  ).

/**********************************************
*************** First approach ****************
***********************************************/

constraints_first_approach(Board) :- 
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

/**********************************************
************** Second approach ****************
***********************************************/

constraints_second_approach(BoardVP2) :-
  alldifferent(BoardVP2),

  ( multifor([J,I],1,9,1),
    param(BoardVP2)
    do
      (I-1)*9 #< BoardVP2[J,I],
      (I-1)*9 + 9 #>= BoardVP2[J,I]
  ),

/**
* This loop can be implemented in a better elegant way
* Number is cell different
**/

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
  ).

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

/**********************************************
*************** Search method *****************
***********************************************/

search(Vars) :-   

  %%once search(Vars,0,input_order, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,anti_first_fail, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,smallest, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,largest, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,occurrence, indomain, complete, [backtrack(A)]),
  %%once search(Vars,0,most_constrained, indomain, complete, [backtrack(A)]),

  %once search(Vars,0,first_fail, indomain_min, complete, [backtrack(A)]),
  %once search(Vars,0,first_fail, indomain_middle, complete, [backtrack(A)]),
  once search(Vars,0,first_fail, indomain_max, complete, [backtrack(A)]),
  write(A - "").
  %%writeln(backtracks - A).

/**********************************************
************** Print solution *****************
***********************************************/

print_solution(Board) :-
( foreacharg(El,Board)
  do
  writeln(El)
).