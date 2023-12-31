:- consult(utils).

% state_char/2
% This predicate is used to convert the state of a cell to a character
state_char(empty, ' ').
state_char(player1, 'X').
state_char(player2, 'O').

/*

            |__ C1 __|__ C2 __|__ C3 __| <- up line
            |        |        |        | <- up2 line
            |        |        |        | <- middle line
            |________|________|________| <- down line

*/

% draw_up_line(+N, +Line, +Column)
% Prints the up line of a cell. Line and Column are used to print the coordinates of the cell.
% N is the number of cells yet to print.
draw_up_line(0, _, _):-
    write('|'), nl.
draw_up_line(N, Line, Column) :-
    N1 is N - 1,
    Column1 is Column + 1,
    write('|'), print_n(2, '_'), write(' '), write(Line), write(Column), write(' '), print_n(2,'_'), draw_up_line(N1, Line, Column1).

% draw_up2_line(+N)
% Prints the up2 line of a cell.
% N is the number of cells yet to print.
draw_up2_line(0):-
    write('|'), nl.
draw_up2_line(N) :-
    N1 is N - 1,
    write('|'), print_n(8, ' '), draw_up2_line(N1).

% draw_down_line(+N)
% Prints the down line of a cell.
% N is the number of cells yet to print.
draw_down_line(0):-
    write('|'), nl.
draw_down_line(N) :-
    N1 is N - 1,
    write('|'), print_n(8, '_'), draw_down_line(N1).

% draw_middle_line(+N, +LineState)
% Prints the middle line of a cell. LineState is a list of the states of the cells in the line.
% N is the number of cells yet to print.
draw_middle_line(0,_):-
    write('|'), nl.
draw_middle_line(N, [H|T]) :-
    state_char(H, Char),
    N1 is N - 1,
    write('|'), print_n(4, ' '), write(Char), print_n(3, ' '), draw_middle_line(N1, T).

% draw_board_line(+DeltaX, +NRet, +LineState, +Letter)
% Prints a line of the board.
% DeltaX is the number of empty spaces to print before the board line.
% NRet is the number of the cells in the line.
% LineState is a list of the states of the cells in the line.
% Letter is the letter of the line.
draw_board_line(DeltaX, NRet, LineState, Letter) :-
    NEspacos is DeltaX * 4,
    print_n(NEspacos, ' '), draw_up_line(NRet, Letter, 1),
    print_n(NEspacos, ' '), draw_up2_line(NRet),
    print_n(NEspacos, ' '), draw_middle_line(NRet, LineState),
    print_n(NEspacos, ' '), draw_down_line(NRet).

% draw_board(+DeltaX, +NRet, +BoardState, +Letter)
% Prints the board.
draw_board(-1, _, _, _).
draw_board(DeltaX, NRet, [Line|Others], Letter) :-
    draw_board_line(DeltaX, NRet, Line, Letter),
    NRet1 is NRet + 1,
    DeltaX1 is DeltaX - 1,
    next_char(Letter, Letter1),
    draw_board(DeltaX1, NRet1, Others, Letter1).

% display_board(+BoardState)
% Prints the board receiving only the board state.
display_board(BoardState) :-
    length(BoardState, N),
    DeltaX is N - 1,
    draw_board(DeltaX, 1, BoardState, 'A').

% in_bounds(+Row, +Col, +BoardSize)
% Checks if the coordinates (Row,Col) are in bounds of the board.
in_bounds(Row, Col, BoardSize):-
    char_code('A', Offset),
    char_code(Row, RowCode),
    RowCode >= Offset,
    RowCode < Offset + BoardSize,
    Col > 0,
    Col =< RowCode - Offset + 1.

% get_piece(+BoardState, +Row, +Col, -Piece)
% Gets the piece in the coordinates (Row,Col) of the board.
get_piece(BoardState, Row,Col, Piece):-
    char_code('A', ACode),
    char_code(Row, RowCode),
    OffsetRow is RowCode - ACode,
    nth0(OffsetRow, BoardState, RowList),
    nth1(Col, RowList, Piece).

% get_adjacent_pieces(+BoardState, +Line, +N, -AdjacentPieces)
% Gets the adjacent pieces of the piece in the coordinates (Line,N) of the board.
% The directions considered are: NO, NE, E, SE, SO, O.
get_adjacent_pieces(BoardState, Line, N, AdjacentPieces):-
    length(BoardState, BoardSize),
    char_code(Line, L),
    L1 is L - 1,
    PrevN is N - 1,
    L2 is L + 1,
    NextN is N + 1,
    char_code(LineAbove, L1),
    char_code(LineBelow, L2),
    (in_bounds(LineAbove, PrevN, BoardSize) -> get_piece(BoardState, LineAbove, PrevN, PieceNO); PieceNO = empty),
    (in_bounds(LineAbove, N, BoardSize) -> get_piece(BoardState, LineAbove, N, PieceNE); PieceNE = empty),
    (in_bounds(Line, NextN, BoardSize) -> get_piece(BoardState, Line, NextN, PieceE); PieceE = empty),
    (in_bounds(LineBelow, NextN, BoardSize) -> get_piece(BoardState, LineBelow, NextN, PieceSE); PieceSE = empty),
    (in_bounds(LineBelow, N, BoardSize) -> get_piece(BoardState, LineBelow, N, PieceSO); PieceSO = empty),
    (in_bounds(Line, PrevN, BoardSize) -> get_piece(BoardState, Line, PrevN, PieceO); PieceO = empty),
    AdjacentPieces = [PieceNO, PieceNE, PieceE, PieceSE, PieceSO, PieceO].

% get_moves_no(+BoardState, +Row, +Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board and direction NO.
get_moves_no(BoardState, Row, Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode - 1,
    char_code(Row1, RowCode1),
    Col1 is Col - 1,
    
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col1, BoardSize) ->
        (get_piece(BoardState, Row1, Col1, Piece),
        (Piece == empty ->
            Moves = [Row1-Col1|Moves1],
            get_moves_no(BoardState, Row1, Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

% get_moves_ne(+BoardState, +Row-Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board and direction NE.
get_moves_ne(BoardState, Row-Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode - 1,
    char_code(Row1, RowCode1),
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col, BoardSize) ->
        (get_piece(BoardState, Row1, Col, Piece),
        (Piece == empty ->
            Moves = [Row1-Col|Moves1],
            get_moves_ne(BoardState, Row1-Col, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

% get_moves_e(+BoardState, +Row-Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board and direction E.
get_moves_e(BoardState, Row-Col, Moves):-
    Col1 is Col + 1,
    length(BoardState, BoardSize),
    (in_bounds(Row, Col1, BoardSize) ->
        (get_piece(BoardState, Row, Col1, Piece),
        (Piece == empty ->
            Moves = [Row-Col1|Moves1],
            get_moves_e(BoardState, Row-Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

% get_moves_se(+BoardState, +Row-Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board and direction SE.
get_moves_se(BoardState, Row-Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode + 1,
    char_code(Row1, RowCode1),
    Col1 is Col + 1,
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col1, BoardSize) ->
        (get_piece(BoardState, Row1, Col1, Piece),
        (Piece == empty ->
            Moves = [Row1-Col1|Moves1],
            get_moves_se(BoardState, Row1-Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

% get_moves_so(+BoardState, +Row-Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board and direction SO.
get_moves_so(BoardState, Row-Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode + 1,
    char_code(Row1, RowCode1),
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col, BoardSize) ->
        (get_piece(BoardState, Row1, Col, Piece),
        (Piece == empty ->
            Moves = [Row1-Col|Moves1],
            get_moves_so(BoardState, Row1-Col, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

% get_moves_o(+BoardState, +Row-Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board and direction O.
get_moves_o(BoardState, Row-Col, Moves):-
    Col1 is Col - 1,
    length(BoardState, BoardSize),
    (in_bounds(Row, Col1, BoardSize) ->
        (get_piece(BoardState, Row, Col1, Piece),
        (Piece == empty ->
            Moves = [Row-Col1|Moves1],
            get_moves_o(BoardState, Row-Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

% get_moves(+BoardState, +Row, +Col, -Moves)
% Gets the possible moves of the piece in the coordinates (Row,Col) of the board.
get_moves(BoardState, Row, Col, Moves):-
    get_moves_no(BoardState, Row, Col, MovesNO),
    get_moves_ne(BoardState, Row-Col, MovesNE),
    get_moves_e(BoardState, Row-Col, MovesE),
    get_moves_se(BoardState, Row-Col, MovesSE),
    get_moves_so(BoardState, Row-Col, MovesSO),
    get_moves_o(BoardState, Row-Col, MovesO),
    append(MovesNO, MovesNE, Moves1),
    append(Moves1, MovesE, Moves2),
    append(Moves2, MovesSE, Moves3),
    append(Moves3, MovesSO, Moves4),
    append(Moves4, MovesO, Moves).
