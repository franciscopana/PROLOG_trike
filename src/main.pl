:- consult('./config.pl').
:- consult('./game.pl').

% play/0
% clears the data, configures the game and begins the game
play:-
    clear_data,
    config(GameState),
    begin_game(GameState).
