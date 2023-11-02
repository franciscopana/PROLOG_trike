:- consult('./board.pl').

display_game(BoardState):-
    display_board(BoardState).

game_cycle([BoardState, Player]):-
    write('Player: '), write(Player), nl,
    write('BoardState: '), write(BoardState), nl,
    display_game(BoardState).