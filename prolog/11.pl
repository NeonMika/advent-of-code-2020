:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(gv)).

:- dynamic(mem_descent/2).

%% -------
%% Helpers
%% -------

% utility predicate to access list members in a certain index range
% index is 0-based
member_between0(Member, Idx, List, Start, End) :-
    between(Start, End, Idx),
    nth0(Idx, List, Member).

%between_inv(+High, +Low, -N).
between_inv(H, L, N) :- 
    L =< H,
    N = H.
between_inv(H, L, N) :- 
    L < H, 
    H1 is H-1, 
    between_inv(H1, L, N).

%% --------
%% Data I/O
%% --------

% input format
place(seat(0, Row, Col, free), Row, Col) --> "L".
place(seat(0, Row, Col, occupied), Row, Col) --> "#".
place(seat(0, Row, Col, floor), Row, Col) --> ".".

input([Place|Rest], Row, Col) --> place(Place, Row, Col), {NewCol is Col + 1, NewRow is Row}, input(Rest, NewRow, NewCol).
input([Place|Rest], Row, Col) --> place(Place, Row, Col), blank, {NewCol is 0, NewRow is Row + 1}, input(Rest, NewRow, NewCol).
input([Place], Row, Col) --> place(Place, Row, Col).

% reads a list of numbers
read_places() :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/11_input.txt", FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(input(Places, 0, 0), FileTextCodes),
    maplist(assertz, Places),
    aggregate_all(max(R), seat(_,R,_,_), MaxRow),
    aggregate_all(max(C), seat(_,_,C,_), MaxCol),
    assertz(max_row(MaxRow)),
    assertz(max_col(MaxCol)).

%% ----------
%% Facts
%% ----------    

%% ----------
%% Predicates
%% ----------     

empty_neighbors1(seat(State, Row, Col, _)) :-
    Row_Min is Row - 1,
    Row_Max is Row + 1,
    Col_Min is Col - 1,
    Col_Max is Col + 1,
    not(seat(State, Row_Min, Col_Min, occupied)), 
    not(seat(State, Row_Min, Col, occupied)), 
    not(seat(State, Row_Min, Col_Max, occupied)), 
    not(seat(State, Row, Col_Min, occupied)), 
    not(seat(State, Row, Col_Max, occupied)), 
    not(seat(State, Row_Max, Col_Min, occupied)), 
    not(seat(State, Row_Max, Col, occupied)), 
    not(seat(State, Row_Max, Col_Max, occupied)).

occupied_count1(seat(State, Row, Col, _), OccupiedCount) :-
    Row_Min is Row - 1,
    Row_Max is Row + 1,
    Col_Min is Col - 1,
    Col_Max is Col + 1,
    aggregate_all(
        count, 
        (
            between(Row_Min, Row_Max, R),
            between(Col_Min, Col_Max, C),
            not((R =:= Row, C =:= Col)),
            seat(State, R, C, occupied)            
        ), 
        OccupiedCount).

seat_step1(Seat) :-
    Seat = seat(State, Row, Col, free), 
    empty_neighbors1(Seat), 
    NewState is (State + 1) mod 2, 
    assertz(seat(NewState, Row, Col, occupied)), 
    !.
seat_step1(Seat) :- 
    Seat = seat(State, Row, Col, occupied), 
    occupied_count1(Seat, OccupiedCount),
    OccupiedCount >= 4,
    NewState is (State + 1) mod 2, 
    assertz(seat(NewState, Row, Col, free)), 
    !.
seat_step1(seat(State, Row, Col, Type)) :- 
    NewState is (State + 1) mod 2, 
    assertz(seat(NewState, Row, Col, Type)).

step1(State) :- foreach(seat(State, R, C, T), seat_step1(seat(State, R, C, T))).

% inefficient, since we always create Offset up until MaxRow / MaxCol even though this may be for out of bounds
% but: it works :D
first_dir([R_Modif, C_Modif], seat(State, Row, Col, _), T) :-
    max_row(MaxRow),
    max_col(MaxCol),
    Dist is min(MaxRow, MaxCol),
    between(1, Dist, Offset),
    R is Row + Offset * R_Modif,
    C is Col + Offset * C_Modif,
    seat(State, R, C, T),
    (T = occupied; T = free),
    !.

first(up,        Seat, T) :- first_dir([-1,  0], Seat, T), !.
first(down,      Seat, T) :- first_dir([ 1,  0], Seat, T), !.
first(left,      Seat, T) :- first_dir([ 0, -1], Seat, T), !.
first(right,     Seat, T) :- first_dir([ 0,  1], Seat, T), !.
first(upleft,    Seat, T) :- first_dir([-1, -1], Seat, T), !.
first(upright,   Seat, T) :- first_dir([-1,  1], Seat, T), !.
first(downleft,  Seat, T) :- first_dir([ 1, -1], Seat, T), !.
first(downright, Seat, T) :- first_dir([ 1,  1], Seat, T), !.
first(_, _, floor).

occupied_count2(Seat, OccupiedCount) :-
    %trace,
    %Seat = seat(_, R, C, _),
    %write(R), write(" "), writeln(C),
    first(up,        Seat, T_Up),
    %writeln(T_Up),
    first(down,      Seat, T_Down),
    %writeln(T_Down),
    first(left,      Seat, T_Left),
    %writeln(T_Left),
    first(right,     Seat, T_Right),
    %writeln(T_Right),
    first(upleft,    Seat, T_UpLeft),
    %writeln(T_UpLeft),
    first(upright,   Seat, T_UpRight),
    %writeln(T_UpRight),
    first(downleft,  Seat, T_DownLeft),
    %writeln(T_DownLeft),
    first(downright, Seat, T_DownRight),
    %writeln(T_DownRight),
    Seen = [T_Up, T_Down, T_Left, T_Right, T_UpLeft, T_UpRight, T_DownLeft, T_DownRight],
    aggregate_all(count, member(occupied, Seen), OccupiedCount).
    %writeln(OccupiedCount). 

seat_step2(Seat) :-
    Seat = seat(State, Row, Col, free), 
    occupied_count2(Seat, 0), 
    NewState is (State + 1) mod 2, 
    assertz(seat(NewState, Row, Col, occupied)), 
    !.
seat_step2(Seat) :- 
    Seat = seat(State, Row, Col, occupied), 
    occupied_count2(Seat, OccupiedCount),
    OccupiedCount >= 5,
    NewState is (State + 1) mod 2, 
    assertz(seat(NewState, Row, Col, free)), 
    !.
seat_step2(seat(State, Row, Col, Type)) :- 
    NewState is (State + 1) mod 2, 
    assertz(seat(NewState, Row, Col, Type)).

step2(State) :- foreach(seat(State, R, C, T), seat_step2(seat(State, R, C, T))).

%% -----
%% Level
%% -----

level(1, N, Result) :-
    Prev is (N + 1) mod 2,
    findall(T, seat(Prev, _, _, T), S1),
    findall(T, seat(N, _, _, T), S2),
    S1 = S2,
    aggregate_all(count, seat(N, _, _ , occupied), Result),
    !.

level(1, N, Result) :-
    M is (N + 1) mod 2,
    retractall(seat(M,_,_,_)), % clear rules
    step1(N),
    level(1, M, Result).

level(2, N, Result) :-
    Prev is (N + 1) mod 2,
    findall(T, seat(Prev, _, _, T), S1),
    findall(T, seat(N, _, _, T), S2),
    S1 = S2,
    aggregate_all(count, seat(N, _, _ , occupied), Result),
    !.

level(2, N, Result) :-
    M is (N + 1) mod 2,
    retractall(seat(M,_,_,_)), % clear rules
    step2(N),
    level(2, M, Result).

%% ----------------
%% Auto-run on load
%% ----------------

%:- set_prolog_stack(global, limit(100 000 000 000)).
%:- set_prolog_stack(trail,  limit(20 000 000 000)).
%:- set_prolog_stack(local,  limit(2 000 000 000)).

:-
    read_places(),
    statistics(runtime,[Start1|_]), level(1, 0, Res1), write('Level 1: '), writeln(Res1), statistics(runtime,[Stop1|_]), Runtime1 is Stop1 - Start1, write('Ran: '), write(Runtime1), writeln('ms'),
    retractall(seat(_,_,_,_)), % clear rules
    read_places(),
    statistics(runtime,[Start2|_]), level(2, 0, Res2), write('Level 2: '), writeln(Res2), statistics(runtime,[Stop2|_]), Runtime2 is Stop2 - Start2, write('Ran: '), write(Runtime2), writeln('ms').