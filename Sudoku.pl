:- use_module(library(clpfd), []).
entry(1).
entry(2).
entry(3).
entry(4).
entry(5).
entry(6).
entry(7).
entry(8).
entry(9).

check_occupiedCell(NewGameBoard, RowIndex, ColumnIndex):-
     % Entries in each row are different.
     find_row(Row, NewGameBoard, RowIndex),
     check_DifferentVals(Row),

     % Entries in each column are different.
     clpfd:transpose(NewGameBoard, Transposed_GameBoard),
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