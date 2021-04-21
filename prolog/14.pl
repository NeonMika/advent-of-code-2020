:- use_module(library(clpfd)).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

level(14).

%% -------
%% Helpers
%% -------

% utility predicate to access list members in a certain index range
% index is 0-based
member_between0(Member, Idx, List, Start, End) :-
    between(Start, End, Idx),
    nth0(Idx, List, Member).

%% ------------------
%% External resources
%% ------------------

%% ------------------
%% Input
%% ------------------

input([mask(MaskData)]) --> 
    "mask = ",
    sequence(nonblank, MaskCodes),
    { convlist([Code, Result]>>(string_codes(String, [Code]), (number_string(Result, String) -> Result = Result; Result = x)), MaskCodes, MaskData) }.

input([mem(Addr, Val)]) -->
    "mem[",
    integer(Addr),
    "] = ",
    integer(Val).

input([mask(MaskData)|T]) --> 
    "mask = ",
    sequence(nonblank, MaskCodes),   
    { convlist([Code, Result]>>(string_codes(String, [Code]), (number_string(Result, String) -> Result = Result; Result = x)), MaskCodes, MaskData) },
    blanks,
    input(T).

input([mem(Addr, Val)|T]) -->
    "mem[",
    integer(Addr),
    "] = ",
    integer(Val),
    blanks,
    input(T).

read_input(Data, Test, Level) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    string_concat("data/", Level, P1),
    string_concat(P1, "_input", P2),
    (Test -> string_concat(P2, "_test", P3); string_concat(P2, "", P3)),
    string_concat(P3, ".txt", Path),
    read_file_to_string(Path, FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(input(Data), FileTextCodes),
    format("Data path: ~w~nContent:~n~w~nRead data:~n~w~n", [Path, FileText, Data]), !.

%% -----------------
%% Actual predicates
%% -----------------

mask(Val, Mask, Masked) :-
    maplist([MaskEle, ValEle, MaskedEle]>>(MaskEle \= x -> MaskedEle = MaskEle; MaskedEle = ValEle), Mask, Val, Masked).    

process(machine(Memory, _), mask(Mask), machine(Memory, Mask)).
process(machine(Memory, Mask), mem(Addr, Val), machine(NewMemory, Mask)) :-
    length(BinaryVal, 36),
    binary_decimal(BinaryVal, Val),
    mask(BinaryVal, Mask, NewMemoryEntry),
    nth0(Addr, Memory, _, Rest),
    nth0(Addr, NewMemory, NewMemoryEntry, Rest).

process_all(M, [], M).
process_all(Machine, [H|T], NewMachine) :-
    process(Machine, H, IntermediateMachine),
    process_all(IntermediateMachine, T, NewMachine).

binary_decimal(BinaryList, Decimal) :-
    reverse(BinaryList, ReversedBinaryList),
    foldl(aggregate_binary, ReversedBinaryList, 0-0, _-Decimal).

aggregate_binary(B, I-A, IN-AN) :-
    B in 0..1, 
    PosVal #= 2 ^ I, 
    AN #= A + PosVal * B, 
    IN #= I + 1.    

%% -----
%% Level
%% -----

level(1, Data, Result) :-
    aggregate_all(max(Addr), member(mem(Addr, _), Data), MaxAddr),
    Length #= MaxAddr + 1,
    length(Memory, Length),
    maplist(=([]), Memory),
    process_all(machine(Memory, []), Data, machine(NewMemory, _)),
    aggregate_all(sum(Val), (member(Cell, NewMemory), binary_decimal(Cell, Val)), Result),
    !.

level(2, Data, Result) :-
    Data = data(_, Lines),    
    findall(-Index-Line, (nth0(Index, Lines, Line), Line \= x), Pairs),
    crt(Pairs, T),
    Result = T, !.

%% ----------------
%% Auto-run on load
%% ----------------

run(test, Stars) :-
    level(Level),
    writeln("Testdata:"),
    read_input(TestData, true, Level),
    statistics(runtime,[TestStart|_]),
    level(Stars, TestData, TestResult), 
    statistics(runtime,[TestStop|_]),
    TestRuntime is TestStop - TestStart,
    format("> Star 1:~n>> Result: ~w~n>> (ran for ~wms)~n", [TestResult, TestRuntime]).

run(real, Stars) :-
    level(Level),
    writeln("Real data:"),
    read_input(Data, false, Level),
    statistics(runtime,[Start|_]),
    level(Stars, Data, Result), 
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start,
    format("> Star 1:~n>> Result: ~w~n>> (ran for ~wms)~n", [Result, Runtime]).

:-  run(test, 1),
    run(real, 1).
    % run(test, 2),
    % run(real, 2).