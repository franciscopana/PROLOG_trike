:-use_module(library(lists)).
:- consult('./board.pl').

other_player(player1,player2).
other_player(player2,player1).

display_game(BoardState):-
    display_board(BoardState).

print_turn(Player):-
    name_of(Player, Name),
    format('\nHey ~w, its your turn!\n', [Name]).

game_cycle([BoardState, Player]):-
    display_game(BoardState),
    print_turn(Player).
    /*
    get_move(BoardState, Player, Move).
*/

in_bounds(Row, Col, BoardSize):-
    char_code('A', Offset),
    char_code(Row, RowCode),
    RowCode >= Offset,
    RowCode < Offset + BoardSize,
    Col > 0,
    Col =< RowCode - Offset + 1.

 
get_inicial_move(Row-Col, BoardState):-
    length(BoardState, BoardSize),
    repeat,
    write('Choose an initial position to move: \n'),
    write('Row: '),
    read(Row),
    write('Column: '),
    read(Col),
    (in_bounds(Row, Col, BoardSize) ->
        ! ;
        write('Invalid move. Please try again.\n'),
        fail
    ).


put_piece(BoardState, Row-Col, Player, NewBoardState):-
    char_code('A', ACode),
    char_code(Row, RowCode),
    OffsetRow is RowCode - ACode,
    nth0(OffsetRow, BoardState, RowList),
    Col1 is Col - 1,
    replace(Col1, Player, RowList, NewRowList),
    replace(OffsetRow, NewRowList, BoardState, NewBoardState).

move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]):-
    put_piece(BoardState, Row-Col, Player, NewBoardState),
    other_player(Player, NewPlayer).

begin_game([BoardState, Player]):-
    display_game(BoardState),
    print_turn(Player),
    get_inicial_move(Row-Col, BoardState),
    move(BoardState, Row-Col, Player, NewGameState),
    game_cycle(NewGameState).
