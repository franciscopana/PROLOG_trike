/*
                        |---A1---|
                        |        |
                        |--------|
                    |---B1---|---B2---|
                    |        |        |
                    |--------|--------|
                |---C1---|---C2---|---C3---|
                |        |        |        |
                |--------|--------|--------|
            |---D1---|---D2---|---D3---|---D4---|
            |        |        |        |        |
            |--------|--------|--------|--------|
        |---E1---|---E2---|---E3---|---E4---|---E5---|
        |        |        |        |        |        |
        |--------|--------|--------|--------|--------|
    |---F1---|---F2---|---F3---|---F4---|---F5---|---F6---|
    |        |        |        |        |        |        |
    |--------|--------|--------|--------|--------|--------|
|---G1---|---G2---|---G3---|---G4---|---G5---|---G6---|---G7---|
|        |        |        |        |        |        |        |
|--------|--------|--------|--------|--------|--------|--------|
*/

state_char(empty, ' ').
state_char(x, 'X').
state_char(o, 'O').

board_state(7, [[o],
                [empty, empty],
                [empty, empty, empty],
                [empty, x, empty, empty],
                [empty, empty, empty, empty, empty],
                [empty, empty, empty, x, empty, empty],
                [empty, o, empty, empty, empty, empty, empty]]).

print_n(0, _).
print_n(N, C):-
    write(C),
    N1 is N - 1,
    print_n(N1, C).

next_char(A, B) :-
    char_code(A, ACode),
    BCode is ACode + 1,
    char_code(B, BCode).

draw_up_line(0, _, _):-
    write('|'), nl.
draw_up_line(N, Line, Column) :-
    N1 is N - 1,
    Column1 is Column + 1,
    write('|'), print_n(2, '_'), write(' '), write(Line), write(Column), write(' '), print_n(2,'_'), draw_up_line(N1, Line, Column1).

draw_up2_line(0):-
    write('|'), nl.
draw_up2_line(N) :-
    N1 is N - 1,
    write('|'), print_n(8, ' '), draw_up2_line(N1).

draw_down_line(0):-
    write('|'), nl.
draw_down_line(N) :-
    N1 is N - 1,
    write('|'), print_n(8, '_'), draw_down_line(N1).

draw_middle_line(0,_):-
    write('|'), nl.
draw_middle_line(N, [H|T]) :-
    state_char(H, Char),
    N1 is N - 1,
    write('|'), print_n(4, ' '), write(Char), print_n(3, ' '), draw_middle_line(N1, T).

draw_board_line(DeltaX, NRet, LineState, Letter) :-
    NEspacos is DeltaX * 4,
    print_n(NEspacos, ' '), draw_up_line(NRet, Letter, 1),
    print_n(NEspacos, ' '), draw_up2_line(NRet),
    print_n(NEspacos, ' '), draw_middle_line(NRet, LineState),
    print_n(NEspacos, ' '), draw_down_line(NRet).

draw_board(-1, _, _, _).
draw_board(DeltaX, NRet, [Line|Others], Letter) :-
    draw_board_line(DeltaX, NRet, Line, Letter),
    NRet1 is NRet + 1,
    DeltaX1 is DeltaX - 1,
    next_char(Letter, Letter1),
    draw_board(DeltaX1, NRet1, Others, Letter1).

draw_board(N) :-
    DeltaX is N - 1,
    board_state(N, BoardState),
    draw_board(DeltaX, 1, BoardState, 'A').


