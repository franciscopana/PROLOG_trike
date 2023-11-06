:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

% clear_data/0
% Clears all dynamic data
clear_data :-
    retractall(name_of(_,_)).

% clear_buffer/0
% Clears input buffer
clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

% clear_console/0
% Clears console
clear_console:- 
    write('\33\[2J').

% get_line(-Result,+Acc)
% Unifies Result with an input line up to endline '\n'
get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).
get_line(Result, Acc):-
    atom_chars(Result, Acc).

% get_name(+Player)
% Asks player name ands saves it in dynamic data using name_of/2
get_name(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    get_line(Name, []),
    asserta(name_of(Player, Name)).

% read_number(-Number)
% Unifies Number with input number from console
read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

% get_option(+Min,+Max,+Context,-Value)
% Unifies Value with the value given by user input between Min and Max when asked about Context
get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

% replace(+Index,+Element,+List,-Result)
% Unifies Result with the list resulting from replace the element at Index of List by Element
replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

% eval(+MiniMaxMode, +Values, -Result)
% Unifies Result with the value according to the MiniMax mode
eval(min, [Value|_], Result):- Result is -Value.
eval(max, Values, Value):- last(Values, Value).

% print_moves(+Moves)
% Prints the list of possible moves
print_moves([]).
print_moves([Row-Col|Moves]):-
    format('~w-~w   ', [Row, Col]),
    print_moves(Moves).

% print_turn_before(+Player)
% Prints the name of the player before his turn
print_turn_before(Player):-
    name_of(Player, Name),
    format('>> Hey ~w, its your turn!\n', [Name]).

% print_turn_after(+Player, +Row-Col)
% Prints the name of the player and the move he chose
print_turn_after(Player, Row-Col):-
    name_of(Player, Name),
    format('\n>> ~w chose ~w-~w\n', [Name, Row, Col]),
    get_char(_).

% print_curr_position(+Row-Col)
% Prints the current position of the player
print_curr_position(Row-Col):-
    format('You are currently on position: ~w-~w\n', [Row, Col]).

% count(+X, +List, -N)
% Counts the number of occurrences of X in List
count(_, [], 0).
count(X, [X|T], N) :-
    count(X, T, N1),
    N is N1 + 1.
count(X, [Y|T], N) :-
    X \= Y,
    count(X, T, N).

% choose(+List, -Elt) 
% chooses a random element from List
choose([], []).
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).

% print_n(+N, +C)
% Prints a character C N times
print_n(0, _).
print_n(N, C):-
    write(C),
    N1 is N - 1,
    print_n(N1, C).

% next_char(+A, -B)
% Unifies B with the next character after A
next_char(A, B) :-
    char_code(A, ACode),
    BCode is ACode + 1,
    char_code(B, BCode).
