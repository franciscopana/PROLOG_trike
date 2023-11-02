/*
                        |__ A1 __|
                        |        |
                        |    O   |
                        |________|
                    |__ B1 __|__ B2 __|
                    |        |        |
                    |        |        |
                    |________|________|
                |__ C1 __|__ C2 __|__ C3 __|
                |        |        |        |
                |        |        |        |
                |________|________|________|
            |__ D1 __|__ D2 __|__ D3 __|__ D4 __|
            |        |        |        |        |
            |        |    X   |        |        |
            |________|________|________|________|
        |__ E1 __|__ E2 __|__ E3 __|__ E4 __|__ E5 __|
        |        |        |        |        |        |
        |        |        |        |        |        |
        |________|________|________|________|________|
    |__ F1 __|__ F2 __|__ F3 __|__ F4 __|__ F5 __|__ F6 __|
    |        |        |        |        |        |        |
    |        |        |        |    X   |        |        |
    |________|________|________|________|________|________|
|__ G1 __|__ G2 __|__ G3 __|__ G4 __|__ G5 __|__ G6 __|__ G7 __|
|        |        |        |        |        |        |        |
|        |    O   |        |        |        |        |        |
|________|________|________|________|________|________|________|
*/

state_char(empty, ' ').
state_char(x, 'X').
state_char(o, 'O').

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

display_board(BoardState) :-
    length(BoardState, N),
    DeltaX is N - 1,
    draw_board(DeltaX, 1, BoardState, 'A').
