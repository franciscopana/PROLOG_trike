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
    congratulate(Winner), nl,nl.

game_cycle([BoardState, Player, CurrRow-CurrCol]):-
    display_game(BoardState),
    get_moves(BoardState, CurrRow, CurrCol, Moves),
    (Moves = [] -> game_over(BoardState, CurrRow, CurrCol, Winner) ; true),
    (Moves \= [] ->
        print_turn_before(Player),
        print_curr_position(CurrRow-CurrCol),
        write('Possible moves: \n'),
        print_moves(Moves),
        get_move(Row-Col, Moves, Player),
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

print_question(Player, Switch):-
    name_of(Player, Bot),
    (Bot = 'bot'; Bot = 'bot1'; Bot = 'bot2'),
    random(0, 2, Switch), 
    (Switch = 1 ->  format('\n>> ~w chose to switch\n', [Bot]);
                    format('\n>> ~w chose not to switch\n', [Bot])).

print_question(Player, Switch):-
    name_of(Player, Name),
    format('\n>> Hey ~w, do you want to switch roles? y/n: ' , [Name]),
    read(Answer),
    (Answer = 'y' ->    Switch = 1, format('\n>> ~w chose to switch\n', [Name]);
                        Switch = 0, format('\n>> ~w chose not to switch\n', [Name])).

begin_game([BoardState, Player]):-
    display_game(BoardState),
    print_turn_before(Player),
    get_initial_move(Row-Col, BoardState, Player),
    print_turn_after(Player, Row-Col),
    other_player(Player, OtherPlayer),
    print_question(OtherPlayer, Switch),
    (Switch = 1 ->  move(BoardState, Row-Col, OtherPlayer, [NewBoardState, NewPlayer]),
                        game_cycle([NewBoardState, Player, Row-Col]);
                    move(BoardState, Row-Col, Player, [NewBoardState, NewPlayer]),
                        game_cycle([NewBoardState, OtherPlayer, Row-Col])).

choose_move(_, Moves, Row-Col, 1, _):-
    choose(Moves, Row-Col).

choose_best_move(_, [Move], Move, _).
choose_best_move(Player, [CurrRow-CurrCol|T], BestMove, BoardState):-
    get_adjacent_pieces(BoardState, CurrRow, CurrCol, AdjacentPieces),
    count(Player, AdjacentPieces, PlayerCount),
    choose_best_move(Player, T, PrevBestMove, BoardState),
    get_adjacent_pieces(BoardState, PrevBestRow, PrevBestCol, PrevAdjacentPieces),
    count(Player, PrevAdjacentPieces, PrevPlayerCount),
    (PlayerCount > PrevPlayerCount -> BestMove = CurrRow-CurrCol; BestMove = PrevBestMove).

choose_move(Player, Moves, Row-Col, 2, BoardState):-
    choose_best_move(Player, Moves, Row-Col, BoardState).

get_move(Row-Col, Moves, Player, BoardState):-
    (name_of(Player, bot);
    name_of(Player, bot1);
    name_of(Player, bot2)),
    difficulty(Player, Difficulty),
    choose_move(Player, Moves, Row-Col, Difficulty, BoardState).

get_move(Row-Col, Moves, Player, _):-
    repeat,
    write('\n\nChoose a position to move to: \n'),
    write('Row: '),
    catch(read(Row), _, (write('Invalid input. Please try again.\n'), fail)),
    write('Column: '),
    catch(read(Col), _, (write('Invalid input. Please try again.\n'), fail)),
    (member(Row-Col, Moves) ->
        ! ;
        write('Invalid move. Please try again.\n'),
        fail
    ).


get_initial_move(Row-Col, BoardState, Player):-
    name_of(Player, bot1),
    length(BoardState, BoardSize), 
    random(0, BoardSize, RowOffset),
    char_code('A', A),
    RowCode is A + RowOffset,
    char_code(Row, RowCode),
    Bruh is RowOffset + 2,
    random(1, Bruh, Col).

get_initial_move(Row-Col, BoardState, Player):-
    length(BoardState, BoardSize),
    repeat,
    write('Choose an initial position to move: \n'),
    write('Row: '),
    catch(read(Row), _, (write('Invalid input. Please try again.\n'), fail)),
    write('Column: '),
    catch(read(Col), _, (write('Invalid input. Please try again.\n'), fail)),
    (in_bounds(Row, Col, BoardSize) ->
        ! ;
        write('Invalid move. Please try again.\n'),
        fail
    ).
    