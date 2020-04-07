entry(1).
entry(2).
entry(3).
entry(4).
entry(5).
entry(6).
entry(7).
entry(8).
entry(9).

sudoku(GameBoard, Solution):-
     sudokuSolver([GameBoard], [], Solution), 
     maplist(portray_clause, Solution),
     !.

sudokuSolver([CurrGameBoard|T], Visited, CurrGameBoard):-
    find_empty_cell(CurrGameBoard, 0, X, Y),
    X = -1.
sudokuSolver([CurrGameBoard|T], Visited, Solution):-
     find_empty_cell(CurrGameBoard, 0, Row, Column),
     getChildren([CurrGameBoard|T], Visited, Row, Column, Children),
     append(T, Children, NewOpen),
     sudokuSolver(NewOpen, [CurrGameBoard|Visited], Solution).

find_empty_cell(_, 9, -1, -1).
find_empty_cell([H|GameBoard], Row, X, Y):-
     (nth0(Index , H, 0)-> X is Row, Y is Index ;
                           Row1 is Row + 1, find_empty_cell(GameBoard, Row1, X, Y)).

getChildren(Open, Visited, Row, Column, Children):-
     findall(X, moves(Open, Visited, Row, Column, X), Children).

moves([CurrGameBoard|T], Visited, Row, Column, NewGameBoard):-
     occupy(CurrGameBoard, Row, Column, NewGameBoard),
     check_occupiedCell(NewGameBoard, Row, Column),
     \+ member(NewGameBoard, [CurrGameBoard|T]),
     \+ member(NewGameBoard, Visited).

occupy(CurrGameBoard, Row, Column, NewGameBoard):-
     replace(CurrGameBoard, Row, Column, NewGameBoard).

replace([H|T], 0, Column, [R|T]):-
     entry(Var),
     replace_column(H, Column, Var, R).  

replace([H|T1], Row, Column, [H|T2]):- 
     Row > 0 ,                              
     Row1 is Row-1 ,                       
     replace(T1 , Row1, Column, T2).         

replace_column([_|T] , 0, Val, [Val|T]).  
replace_column([H|T1], Column, Val, [H|T2]) :- 
     Column > 0,                                
     Column1 is Column-1,                                
     replace_column(T1 , Column1, Val, T2).

check_occupiedCell(NewGameBoard, RowIndex, ColumnIndex):-
     % Entries in each row are different.
     find_row(Row, NewGameBoard, RowIndex),
     check_DifferentVals(Row),

     % Entries in each column are different.
     transpose(NewGameBoard, Transposed_GameBoard),
     find_row(Column, Transposed_GameBoard, ColumnIndex),
     check_DifferentVals(Column),

     % Entries in each 3x3-block are different.
     check_DifferentBlocks(NewGameBoard).

% Checks that all the elements of the given list have different values.
check_DifferentVals([]).
check_DifferentVals([H|T]):-
     H = 0,
     check_DifferentVals(T).

check_DifferentVals([H|T]):-
     \+ member(H, T),
     check_DifferentVals(T).

check_DifferentBlocks([]).
check_DifferentBlocks([R1, R2, R3|GameBoard]):-
     divide_into_blocks(R1, R2, R3).

divide_into_blocks([], [], []).
divide_into_blocks([A, B, C|R1],
                   [D, E, F|R2],
                   [G, H, I|R3]):-
     check_DifferentVals([A, B, C, D, E, F, G, H, I]),
     divide_into_blocks(R1, R2, R3).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).