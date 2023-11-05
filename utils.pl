:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

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
    char_code('A', ACode),
    char_code(Row1, RowCode),
    OffsetRow is RowCode - ACode,
    nth0(OffsetRow, BoardState, RowList),
    Col2 is Col1 - 1,
    nth0(Col2, RowList, Piece).

get_adjacent_pieces(BoardState, Line, N, AdjacentPieces):-
    length(BoardState, BoardSize),
    char_code(Line, L),
    L1 is L - 1,
    PrevN is N - 1,
    L2 is L + 1,
    NextN is N + 1,
    char_code(LineAbove, L1),
    char_code(LineBelow, L2),
    (in_bounds(LineAbove, PrevN, BoardSize) -> get_piece(BoardState, LineAbove, PrevN, PieceNO); PieceNO = empty),
    (in_bounds(LineAbove, N, BoardSize) -> get_piece(BoardState, LineAbove, N, PieceNE); PieceNE = empty),
    (in_bounds(Line, NextN, BoardSize) -> get_piece(BoardState, Line, NextN, PieceE); PieceE = empty),
    (in_bounds(LineBelow, NextN, BoardSize) -> get_piece(BoardState, LineBelow, NextN, PieceSE); PieceSE = empty),
    (in_bounds(LineBelow, N, BoardSize) -> get_piece(BoardState, LineBelow, N, PieceSO); PieceSO = empty),
    (in_bounds(Line, PrevN, BoardSize) -> get_piece(BoardState, Line, PrevN, PieceO); PieceO = empty),
    AdjacentPieces = [PieceNO, PieceNE, PieceE, PieceSE, PieceSO, PieceO].


get_moves_no(BoardState, Row, Col, Moves):-
    char_code(Row, RowCode),
    RowCode1 is RowCode - 1,
    char_code(Row1, RowCode1),
    Col1 is Col - 1,
    
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


get_initial_move(Row-Col, BoardState, Player):-
    name_of(Player, bot1),
    length(BoardState, BoardSize), 
    random(0, BoardSize, RowOffset),
    char_code('A', A),
    RowCode is A + RowOffset,
    char_code(Row, RowCode),
    Bruh is RowOffset + 2,
    random(1, Bruh, Col).

get_initial_move(Row-Col, BoardState, Player):-
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
    /* wait for user to press enter */
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
