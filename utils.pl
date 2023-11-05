:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

clear_data :-
    retractall(name_of(_,_)),
    retractall(state_char(_,_)).

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

% abs(+Number,-AbsNumber)
% Unifies AbsNumber with the absolute value of Number
abs(X,X) :- X >= 0, !.
abs(X,Y) :- Y is -X.

% get_name(+Player)
% Asks player name. Dynamically adds the name_of/2 fact to the base fact
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

% swap_minimax(+MiniMaxMode, -NewMode)
% Swaps minimax algorithm mode
swap_minimax(min, max).
swap_minimax(max, min).

% eval(+MiniMaxMode, +Values, -Result)
% Unifies Result with the value according to the MiniMax mode
eval(min, [Value|_], Result):- Result is -Value.
eval(max, Values, Value):- last(Values, Value).

get_move(Row-Col, Moves, Player):-
    (name_of(Player, bot);
    name_of(Player, bot1);
    name_of(Player, bot2)),
    choose(Moves, Row-Col).
get_move(Row-Col, Moves, Player):-
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

print_moves([]).
print_moves([Row-Col|Moves]):-
    format('~w-~w   ', [Row, Col]),
    print_moves(Moves).

print_turn_before(Player):-
    name_of(Player, Name),
    format('>> Hey ~w, its your turn!\n', [Name]).

print_turn_after(Player, Row-Col):-
    name_of(Player, Name),
    format('\n>> ~w chose ~w-~w\n', [Name, Row, Col]),
    get_char(_).

print_curr_position(Row-Col):-
    format('You are currently on position: ~w-~w\n', [Row, Col]).


count(_, [], 0).
count(X, [X|T], N) :-
    count(X, T, N1),
    N is N1 + 1.
count(X, [Y|T], N) :-
    X \= Y,
    count(X, T, N).


%% choose(List, Elt) - chooses a random element
%% in List and unifies it with Elt.
choose([], []).
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).

print_n(0, _).
print_n(N, C):-
    write(C),
    N1 is N - 1,
    print_n(N1, C).

next_char(A, B) :-
    char_code(A, ACode),
    BCode is ACode + 1,
    char_code(B, BCode).