:- consult('./config.pl').
:- consult('./game.pl').

play:-
    clear_data,
    config(GameState),
    begin_game(GameState).
