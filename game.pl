:- consult('./board.pl').

display_game(BoardState):-
    display_board(BoardState).

print_turn(Player):-
    name_of(Player, Name),
    format('\nHey ~w, its your turn!\n', [Name]).

game_cycle([BoardState, Player]):-
    display_game(BoardState),
    print_turn(Player),
    get_move(BoardState, Player, Move).
