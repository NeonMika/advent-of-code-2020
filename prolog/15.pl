:- use_module(library(clpfd)).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module(library(yall)).

:- set_prolog_flag(history, 50).

level(15).

%% -------
%% Helpers
%% -------

% utility predicate to access list members in a certain index range
% index is 0-based
member_between0(Member, Idx, List, Start, End) :-
    between(Start, End, Idx),
    nth0(Idx, List, Member).

binary_decimal(BinaryList, Decimal) :-
    length(BinaryList, 36),
    reverse(BinaryList, ReversedBinaryList),
    foldl(aggregate_binary, ReversedBinaryList, 0-0, _-Decimal).

aggregate_binary(B, I-A, IN-AN) :-
    B in 0..1, 
    PosVal #= 2 ^ I, 
    AN #= A + PosVal * B, 
    IN #= I + 1.    

%% ------------------
%% External resources
%% ------------------

%% ------------------
%% Input
%% ------------------

read_input(Data, Test, Level, Stars) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    string_concat("data/", Level, P1),
    string_concat(P1, "_input", P2),
    (Test -> string_concat(P2, "_test", PX), string_concat(PX, Stars, P3); string_concat(P2, "", P3)),
    string_concat(P3, ".txt", Path),
    read_file_to_string(Path, FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(sequence(integer, ",", Data), FileTextCodes),
    format("Data path: ~w~nContent:~n~w~nRead data:~n~w~n", [Path, FileText, Data]), !.

%% -----------------
%% Actual predicates
%% -----------------
search(Assoc, X, F, T, X) :- F > T, !.
search(Assoc, LastNumber, From, To, LastSpokenNumber) :-
    (0 is mod(From, 10_000) -> writeln(From); true),
    next(Assoc, LastNumber, From, WrittenNumber, NewAssoc),
    NewFrom #= From + 1,
    search(NewAssoc, WrittenNumber, NewFrom, To, LastSpokenNumber).

next(Assoc, LastNumber, Pos, WrittenNumber, NewAssoc) :- 
    get_assoc(LastNumber, Assoc, Positions),
    Positions = Last//Previous,
    Previous #\= -1,
    WrittenNumber #= Last - Previous,
    (get_assoc(WrittenNumber, Assoc, LastWrittenNumber//_) -> put_assoc(WrittenNumber, Assoc, Pos//LastWrittenNumber, NewAssoc); put_assoc(WrittenNumber, Assoc, Pos//(-1), NewAssoc)).

next(Assoc, LastNumber, Pos, WrittenNumber, NewAssoc) :- 
    get_assoc(LastNumber, Assoc, Positions),
    Positions = Last//Previous,
    Previous #= -1,
    WrittenNumber #= 0,
    get_assoc(WrittenNumber, Assoc, LastZero//_),
    put_assoc(WrittenNumber, Assoc, Pos//LastZero, NewAssoc).

numbers_assoc(List, Assoc) :-
    findall((X)-(I)//(-1), nth1(I, List, X), Pairs),    
    list_to_assoc(Pairs, Assoc).
%% -----
%% Level
%% -----

level(1, Data, Result) :-
    length(Data, NStartingNumbers),
    last(Data, LastNumber),
    numbers_assoc(Data, Assoc),
    StartRound #= NStartingNumbers + 1,
    search(Assoc, LastNumber, StartRound, 2020, LastSpokenNumber),
    Result = LastSpokenNumber.

level(2, Data, Result) :-    
    length(Data, NStartingNumbers),
    last(Data, LastNumber),
    numbers_assoc(Data, Assoc),
    StartRound #= NStartingNumbers + 1,
    search(Assoc, LastNumber, StartRound, 30_000_000, LastSpokenNumber),
    Result = LastSpokenNumber.

%% ----------------
%% Auto-run on load
%% ----------------

run(test, Stars) :-
    level(Level),
    writeln("Testdata:"),
    read_input(TestData, true, Level, Stars),
    statistics(runtime,[TestStart|_]),
    level(Stars, TestData, TestResult), 
    statistics(runtime,[TestStop|_]),
    TestRuntime is TestStop - TestStart,
    format("> Star ~w:~n>> Result: ~w~n>> (ran for ~wms)~n", [Stars, TestResult, TestRuntime]).

run(real, Stars) :-
    level(Level),
    writeln("Real data:"),
    read_input(Data, false, Level, Stars),
    statistics(runtime,[Start|_]),
    level(Stars, Data, Result), 
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start,
    format("> Star ~w:~n>> Result: ~w~n>> (ran for ~wms)~n", [Stars, Result, Runtime]).

:-  set_prolog_flag(stack_limit, 4_000_000_000),
    %profile(run(test, 1)),
    %profile(run(real, 1)),
    %profile(run(test, 2)),
    %profile(run(real, 2)),
    run(test, 1),
    run(real, 1),
    %run(test, 2),
    run(real, 2).