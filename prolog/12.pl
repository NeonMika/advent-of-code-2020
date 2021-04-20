:- use_module(library(clpfd)).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% input
input([ operation(Op, Val) ]) --> operation(Op, Val).
input([ operation(Op, Val) | Tail]) --> operation(Op, Val), blanks, input(Tail).

operation(OpChar, Val) --> alpha_to_lower(Op), integer(Val), { char_code(OpChar, Op) }.

read_input(Data, Test, Level) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    string_concat("data/", Level, P1),
    string_concat(P1, "_input", P2),
    (Test -> string_concat(P2, "_test", P3); string_concat(P2, "", P3)),
    string_concat(P3, ".txt", Path),
    write("Data path: "),
    writeln(Path),
    read_file_to_string(Path, FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(input(Data), FileTextCodes), !.

% right rotation definitions
rot(n, e).
rot(e, s).
rot(s, w).
rot(w, n).

% process(ShipBefore, Operation, ShipAfter)
process(1, ship(X, Y, Dir), operation(n, Val), ship(X, NewY, Dir)) :- NewY #= Y + Val.
process(1, ship(X, Y, Dir), operation(s, Val), ship(X, NewY, Dir)) :- NewY #= Y - Val.
process(1, ship(X, Y, Dir), operation(e, Val), ship(NewX, Y, Dir)) :- NewX #= X + Val.
process(1, ship(X, Y, Dir), operation(w, Val), ship(NewX, Y, Dir)) :- NewX #= X - Val.

process(1, Ship, operation(l, 0), Ship) :- !.
process(1, ship(X, Y, Dir), operation(l, Val), ShipAfter) :- 
    rot(StepDir, Dir),
    StepVal #= Val mod 360 - 90,
    process(1, ship(X, Y, StepDir), operation(l, StepVal), ShipAfter), !.
process(1, ShipBefore, operation(r, Val), ShipAfter) :-
    LeftVal #= 360 - Val mod 360,
    process(1, ShipBefore, operation(l, LeftVal), ShipAfter), !.

process(1, ship(X, Y, Dir), operation(f, Val), ShipAfter) :- process(1, ship(X, Y, Dir), operation(Dir, Val), ShipAfter). 

process(2, ship(X, Y, WPX, WPY), operation(n, Val), ship(X, Y, WPX, NWPY)) :- NWPY #= WPY + Val.
process(2, ship(X, Y, WPX, WPY), operation(s, Val), ship(X, Y, WPX, NWPY)) :- NWPY #= WPY - Val.
process(2, ship(X, Y, WPX, WPY), operation(e, Val), ship(X, Y, NWPX, WPY)) :- NWPX #= WPX + Val.
process(2, ship(X, Y, WPX, WPY), operation(w, Val), ship(X, Y, NWPX, WPY)) :- NWPX #= WPX - Val.

process(2, Ship, operation(r, 0), Ship) :- !.
process(2, ship(X, Y, WPX, WPY), operation(r, Val), ship(X, Y, NWPX, NWPY)) :- 
    Deg #= Val mod 360,
    Rad is Deg / 180 * pi,
    NWPX is round(WPX * cos(Rad) + WPY * sin(Rad)),
    NWPY is round(-WPX * sin(Rad) + WPY * cos(Rad)).
process(2, ShipBefore, operation(l, Val), ShipAfter) :-
    RightVal #= 360 - Val mod 360,
    process(2, ShipBefore, operation(r, RightVal), ShipAfter), !.

process(2, Ship, operation(f, 0), Ship) :- !.
process(2, ship(X, Y, WPX, WPY), operation(f, Val), ShipAfter) :- 
    NX #= X + WPX,
    NY #= Y + WPY,
    NVal #= Val - 1,
    process(2, ship(NX, NY, WPX, WPY), operation(f, NVal), ShipAfter). 

% process_all
process_all(_, Ship, [], Ship).
process_all(Level, Ship, [Op|T], ShipAfter) :-
    write(Ship), write(" "), writeln(Op),
    process(Level, Ship, Op, ShipIntermediate),
    process_all(Level, ShipIntermediate, T, ShipAfter).

%% -----
%% Level
%% -----

level(1, Operations, Result) :-
    process_all(1, ship(0, 0, e), Operations, ship(XEnd, YEnd, _)),
    Result #= abs(XEnd) + abs(YEnd).

level(2, Operations, Result) :- 
    process_all(2, ship(0, 0, 10, 1), Operations, ship(XEnd, YEnd, _, _)),
    Result #= abs(XEnd) + abs(YEnd).

%% ----------------
%% Auto-run on load
%% ----------------

:-
    read_input(TestData, true, 12),
    statistics(runtime,[TestStart1|_]),
    level(1, TestData, TestResult1), 
    write('Level 1 (Testdata): '), writeln(TestResult1),
    statistics(runtime,[TestStop1|_]),
    TestRuntime1 is TestStop1 - TestStart1,
    write('Ran: '), write(TestRuntime1), writeln('ms'),
    statistics(runtime,[TestStart2|_]),
    level(2, TestData, TestResult2), 
    write('Level 2 (Testdata): '), writeln(TestResult2),
    statistics(runtime,[TestStop2|_]),
    TestRuntime2 is TestStop2 - TestStart2,
    write('Ran: '), write(TestRuntime2), writeln('ms'),

    read_input(Data, false, 12),
    statistics(runtime,[Start1|_]),
    level(1, Data, Res1), write('Level 1: '), writeln(Res1),
    statistics(runtime,[Stop1|_]),
    Runtime1 is Stop1 - Start1,
    write('Ran: '), write(Runtime1), writeln('ms'),
    statistics(runtime,[Start2|_]),
    level(2, Data, Res2), write('Level 2: '), writeln(Res2),
    statistics(runtime,[Stop2|_]),
    Runtime2 is Stop2 - Start2,
    write('Ran: '), write(Runtime2), writeln('ms').

%% -------
%% Helpers
%% -------

% utility predicate to access list members in a certain index range
% index is 0-based
member_between0(Member, Idx, List, Start, End) :-
    between(Start, End, Idx),
    nth0(Idx, List, Member).
