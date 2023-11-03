:-use_module(library(lists)).
:- consult('./board.pl').

other_player(player1,player2).
other_player(player2,player1).

display_game(BoardState):-
    display_board(BoardState).

get_adjacent_pieces(BoardState, Letra, N, AdjacentPieces):-
    length(BoardState, BoardSize),
    /*Check if the Letra and N are inbound with the in_bounds function*/
    char_code(Letra, L),
    L1 is L - 1,
    N1 is N - 1,
    L2 is L + 1,
    N2 is N + 1,
    

    char_code(Letra1, L1),
    char_code(Letra2, L2),
    get_piece(BoardState, Letra1, N1, Piece1),
    write('Piece1: '), write(Piece1), nl,
    get_piece(BoardState, Letra1, N, Piece2),
    write('Piece2: '), write(Piece2), nl,
    get_piece(BoardState, L, N2, Piece3),
    write('Piece3: '), write(Piece3), nl,
    get_piece(BoardState, Letra2, N2, Piece4),
    write('Piece4: '), write(Piece4), nl,
    get_piece(BoardState, Letra2, N, Piece5),
    write('Piece5: '), write(Piece5), nl,
    get_piece(BoardState, L, N1, Piece6),
    write('Piece6: '), write(Piece6), nl,
    append([Piece1, Piece2, Piece3, Piece4, Piece5, Piece6], AdjacentPieces),
    write('Adjacent pieces: '), write(AdjacentPieces), nl.

check_winner(BoardState, CurrX, CurrY, Winner):-
    get_piece(BoardState, CurrX, CurrY, Piece),
    get_adjacent_pieces(BoardState, CurrX, CurrY, AdjacentPieces),
    count(AdjacentPieces, player1, Player1Count),
    count(AdjacentPieces, player2, Player2Count),
    write('Player1 count: '), write(Player1Count), nl,
    write('Player2 count: '), write(Player2Count), nl,
    /*if player1 count == player2 count, the winner is Piece*/
    (Player1Count > Player2Count -> Winner = player1
    ; Player1Count < Player2Count -> Winner = player2
    ; Winner = Piece).

congratulate(Winner):-
    name_of(Winner, Name),
    format('\nCongratulations ~w, you won!', [Name]).

game_over(BoardState, CurrX, CurrY, Winner):-
    write('Game Over!\n'),
    check_winner(BoardState, CurrX, CurrY, Winner),
    congratulate(Winner).


game_cycle([BoardState, Player, CurrRow-CurrCol]):-
    display_game(BoardState),
    print_turn(Player),
    print_curr_position(CurrRow-CurrCol),
    get_moves(BoardState, CurrRow, CurrCol, Moves),
    (Moves = [] -> game_over(BoardState, CurrRow, CurrCol, Winner) ; true),
    (Moves \= [] ->
        write('Possible moves: \n'),
        print_moves(Moves),
        get_move(Row-Col, Moves),
        move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]),
        game_cycle([NewBoardState, NewPlayer, Row-Col])
    ; true).

put_piece(BoardState, Row-Col, Piece, NewBoardState):-
    char_code('A', ACode),
    char_code(Row, RowCode),
    OffsetRow is RowCode - ACode,
    nth0(OffsetRow, BoardState, RowList),
    Col1 is Col - 1,
    replace(Col1, Piece, RowList, NewRowList),
    replace(OffsetRow, NewRowList, BoardState, NewBoardState).

move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]):-
    put_piece(BoardState, Row-Col, Player, NewBoardState),
    other_player(Player, NewPlayer).

print_question(Player, Answer):-
    name_of(Player, Name),
    format('\nHey ~w, do you want to play as X or O?', [Name]),
    read(Answer).

begin_game([BoardState, Player]):-
    display_game(BoardState),
    print_turn(Player),
    get_inicial_move(Row-Col, BoardState),
    move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]),
    game_cycle([NewBoardState, NewPlayer, Row-Col]).
