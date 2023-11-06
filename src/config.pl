:- consult(utils).

% choose_difficulty(+Bot)
% Choose Bot difficulty (1 for Rabdom or 2 for Greedy)
choose_difficulty(Bot) :-
    format('\nPlease select ~a difficulty:\n', [Bot]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).

% menu_option(+N)
% Main menu options. Each represents a game mode.
menu_option(1):-
    nl, write('Human vs. Human\n'), nl,
    get_name(player1), nl, get_name(player2).
menu_option(2):-
    write('Human vs. Bot\n'),
    get_name(player1),
    asserta((name_of(player2, 'bot'))), !, 
    choose_difficulty(player2).
menu_option(3):-
    write('Bot vs. Bot\n'),
    asserta((name_of(player1, 'bot1'))),
    asserta((name_of(player2, 'bot2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

% trike/0
% Game header
trike:-
    clear_console,
    write('||======================||\n'),
    write('||   Welcome to Trike!  ||\n'),
    write('||======================||\n'), nl.

% menu/0
% Main menu
menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n\n').

% set_mode/0
% Asks the user to choose a game mode
set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    menu_option(Option).

% choose_board(-Size)
% Board size choice
choose_board(Size):-
    nl, write('Board size: 6 - 9? '),
    repeat,
    read_number(Size),
    member(Size, [6,7,8,9]), !.

% init_state(+Size, -Board)
% Initialize Board with an empty board of size Size
init_state(Size, Board) :-
    init_state(Size, [], Board).
init_state(0, Acc, Acc).
init_state(Size, Acc, Board) :-
    Size > 0,
    length(Row, Size),
    maplist(=(empty), Row),
    NewSize is Size - 1,
    init_state(NewSize, [Row|Acc], Board).

% configuration(-GameState)
% Initialize GameState with an empty board and player1 as the starting player
config([Board,player1]):-
    trike,
    set_mode,
    choose_board(Size),
    init_state(Size, Board).
