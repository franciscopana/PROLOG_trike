/*
               x=4
                |--------|
                |        |
           x=3  |--------|
            |--------|--------|
            |        |        |
            |--------|--------|
        |--------|--------|--------|
        |        |        |        |
        |--------|--------|--------|
    |--------|--------|--------|--------|
    |        |        |        |        |
    |--------|--------|--------|--------|
|--------|--------|--------|--------|--------|
|        |        |        |        |        |
|--------|--------|--------|--------|--------|
*/

print_n(0, _).
print_n(N, C):-
    write(C),
    N1 is N - 1,
    print_n(N1, C).


draw_ud_line(0):-
    write('|'), nl.
draw_ud_line(N) :-
    N1 is N - 1,
    write('|'), print_n(8, '-'), draw_ud_line(N1).

draw_middle_line(0):-
    write('|'), nl.
draw_middle_line(N) :-
    N1 is N - 1,
    write('|'), print_n(8, ' '), draw_middle_line(N1).

draw_rectangles(N):-
    draw_ud_line(N),
    draw_middle_line(N),
    draw_ud_line(N).

draw_board_line(DeltaX, NRet) :-
    NEspacos is DeltaX * 4,
    print_n(NEspacos, ' '),
    draw_ud_line(NRet),
    print_n(NEspacos, ' '),
    draw_middle_line(NRet),
    print_n(NEspacos, ' '),
    draw_ud_line(NRet).

draw_board(0, NRet) :- draw_board_line(0, NRet).
draw_board(DeltaX, NRet) :-
    draw_board_line(DeltaX, NRet),
    NRet1 is NRet + 1,
    DeltaX1 is DeltaX - 1,
    draw_board(DeltaX1, NRet1).

draw_board(N) :-
    DeltaX is N - 1,
    draw_board(DeltaX, 1).