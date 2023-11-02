:- consult('./config.pl').
:- consult('./game.pl').

play_game :-
    clear_data,
    config(GameState),
    begin_game(GameState).
