:- consult('./board.pl').

other_player(player1,player2).
other_player(player2,player1).

display_game(BoardState):-
    write('\n************************************\n\n'),
    display_board(BoardState), nl.

check_winner(BoardState, Line, N, Winner):-
    get_piece(BoardState, Line, N, Piece),
    get_adjacent_pieces(BoardState, Line, N, AdjacentPieces),
    count(player1, AdjacentPieces, Player1Count),
    count(player2, AdjacentPieces, Player2Count),
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
    get_moves(BoardState, CurrRow, CurrCol, Moves),
    (Moves = [] -> game_over(BoardState, CurrRow, CurrCol, Winner) ; true),
    (Moves \= [] ->
        print_turn(Player),
        print_curr_position(CurrRow-CurrCol),
        write('Possible moves: \n'),
        print_moves(Moves),
        get_move(Row-Col, Moves),
        move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]),
        print_turn_after(Player, Row-Col),
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
    print_turn_before(Player),
    get_inicial_move(Row-Col, BoardState),
    move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]),
    print_turn_after(Player, Row-Col),
    game_cycle([NewBoardState, NewPlayer, Row-Col]).
