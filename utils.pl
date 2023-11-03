:- use_module(library(between)).

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


in_bounds(Row, Col, BoardSize):-
    char_code('A', Offset),
    char_code(Row, RowCode),
    RowCode >= Offset,
    RowCode < Offset + BoardSize,
    Col > 0,
    Col =< RowCode - Offset + 1.

get_piece(BoardState, Row1,Col1, Piece):-
    length(BoardState, BoardSize),
    /*Check if the coordinates are inbound, otherwise return invalid Piece*/
    (in_bounds(Row1, Col1, BoardSize) ->
        true ;
        Piece = invalid
    ),
    char_code('A', ACode),
    char_code(Row1, RowCode),
    OffsetRow is RowCode - ACode,
    nth0(OffsetRow, BoardState, RowList),
    Col2 is Col1 - 1,
    nth0(Col2, RowList, Piece).


get_moves_no(BoardState, Row, Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode - 1, /*diminui a letra em 1 */
    char_code(Row1, RowCode1),/*Row1 é a letra diminuida*/
    Col1 is Col - 1, /*Col1 é a coluna diminuida*/
    
    /*Now i need to check if the position is empty and valid*/
    /*If it is, i add it to the list of moves*/
    /*If it is not, i just return the current moves acumulated and stop*/
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col1, BoardSize) ->
        (get_piece(BoardState, Row1, Col1, Piece),
        (Piece == empty ->
            Moves = [Row1-Col1|Moves1],
            get_moves_no(BoardState, Row1, Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

get_moves_ne(BoardState, Row-Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode - 1,
    char_code(Row1, RowCode1),
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col, BoardSize) ->
        (get_piece(BoardState, Row1, Col, Piece),
        (Piece == empty ->
            Moves = [Row1-Col|Moves1],
            get_moves_ne(BoardState, Row1-Col, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

get_moves_e(BoardState, Row-Col, Moves):-
    Col1 is Col + 1,
    length(BoardState, BoardSize),
    (in_bounds(Row, Col1, BoardSize) ->
        (get_piece(BoardState, Row, Col1, Piece),
        (Piece == empty ->
            Moves = [Row-Col1|Moves1],
            get_moves_e(BoardState, Row-Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

get_moves_se(BoardState, Row-Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode + 1,
    char_code(Row1, RowCode1),
    Col1 is Col + 1,
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col1, BoardSize) ->
        (get_piece(BoardState, Row1, Col1, Piece),
        (Piece == empty ->
            Moves = [Row1-Col1|Moves1],
            get_moves_se(BoardState, Row1-Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

get_moves_so(BoardState, Row-Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode + 1,
    char_code(Row1, RowCode1),
    length(BoardState, BoardSize),
    (in_bounds(Row1, Col, BoardSize) ->
        (get_piece(BoardState, Row1, Col, Piece),
        (Piece == empty ->
            Moves = [Row1-Col|Moves1],
            get_moves_so(BoardState, Row1-Col, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

get_moves_o(BoardState, Row-Col, Moves):-
    Col1 is Col - 1,
    length(BoardState, BoardSize),
    (in_bounds(Row, Col1, BoardSize) ->
        (get_piece(BoardState, Row, Col1, Piece),
        (Piece == empty ->
            Moves = [Row-Col1|Moves1],
            get_moves_o(BoardState, Row-Col1, Moves1) ;
            Moves = [])
        ) ;
        Moves = []
    ).

get_moves(BoardState, Row, Col, Moves):-
    get_moves_no(BoardState, Row, Col, MovesNO),
    get_moves_ne(BoardState, Row-Col, MovesNE),
    get_moves_e(BoardState, Row-Col, MovesE),
    get_moves_se(BoardState, Row-Col, MovesSE),
    get_moves_so(BoardState, Row-Col, MovesSO),
    get_moves_o(BoardState, Row-Col, MovesO),
    append(MovesNO, MovesNE, Moves1),
    append(Moves1, MovesE, Moves2),
    append(Moves2, MovesSE, Moves3),
    append(Moves3, MovesSO, Moves4),
    append(Moves4, MovesO, Moves).

get_inicial_move(Row-Col, BoardState):-
    length(BoardState, BoardSize),
    repeat,
    write('Choose an initial position to move: \n'),
    write('Row: '),
    read(Row),
    write('Column: '),
    read(Col),
    (in_bounds(Row, Col, BoardSize) ->
        ! ;
        write('Invalid move. Please try again.\n'),
        fail
    ).

get_move(Row-Col, Moves):-
    /*ask the user for row and column*/
    repeat,
    write('\nChoose a position to move to: \n'),
    write('Row: '),
    read(Row),
    write('Column: '),
    read(Col),
    (member(Row-Col, Moves) ->
        ! ;
        write('Invalid move. Please try again.\n'),
        fail
    ).

print_moves([]).
print_moves([Row-Col|Moves]):-
    format('~w-~w   ', [Row, Col]),
    print_moves(Moves).

print_turn(Player):-
    name_of(Player, Name),
    format('\nHey ~w, its your turn!\n', [Name]).

print_curr_position(Row-Col):-
    format('You are currently on position: ~w-~w\n', [Row, Col]).