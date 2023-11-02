:- consult('./config.pl').
:- consult('./game.pl').

play_game :-
    config(GameState),
    game_cycle(GameState).