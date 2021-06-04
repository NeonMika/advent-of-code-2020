:- use_module(library(clpfd)).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module(library(yall)).

:- set_prolog_flag(history, 50).

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

read_input(Data, Test, Level, Stars) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    string_concat("data/", Level, P1),
    string_concat(P1, "_input", P2),
    (Test -> string_concat(P2, "_test", PX), string_concat(PX, Stars, P3); string_concat(P2, "", P3)),
    string_concat(P3, ".txt", Path),
    read_file_to_string(Path, FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(input(Data), FileTextCodes),
    format("Data path: ~w~nContent:~n~w~nRead data:~n~w~n", [Path, FileText, Data]), !.

%% -----------------
%% Actual predicates
%% -----------------

mask_value(Val, Mask, Masked) :-
    maplist([MaskEle, ValEle, MaskedEle]>>(MaskEle \= x -> MaskedEle = MaskEle; MaskedEle = ValEle), Mask, Val, Masked).

mask_addr(Addr, Mask, Masked) :-
    maplist([MaskEle, AddrEle, MaskedEle]>>(MaskEle = 0 -> MaskedEle = AddrEle; MaskedEle = MaskEle), Mask, Addr, Masked).

process(_, machine(Memory, _), mask(Mask), machine(Memory, Mask)).
process(1, machine(Memory, Mask), mem(Addr, Val), machine(NewMemory, Mask)) :-
    binary_decimal(BinaryVal, Val),
    mask_value(BinaryVal, Mask, NewBinaryMemoryEntry),
    NewEntry = Addr-NewBinaryMemoryEntry,
    (member(Addr-_, Memory) -> select(Addr-_, Memory, NewEntry, NewMemory); select(NewEntry, NewMemory, Memory)).
process(2, machine(Memory, Mask), mem(Addr, Val), machine(NewMemory, Mask)) :-
    binary_decimal(BinaryVal, Val),
    binary_decimal(BinaryAddr, Addr),
    mask_addr(BinaryAddr, Mask, AddrX),
    findall(OverwrittenAddr, (addrx_addr(AddrX, OverwrittenBinaryAddr), binary_decimal(OverwrittenBinaryAddr, OverwrittenAddr)), OverwrittenAddrs),
    maplist({BinaryVal}/[A, Entry]>>(Entry=A-BinaryVal), OverwrittenAddrs, NewMemoryParts),
    include({OverwrittenAddrs}/[A-_]>>(\+member(A, OverwrittenAddrs)), Memory, SurvivingMemoryParts),
    append(NewMemoryParts, SurvivingMemoryParts, NewMemory).
    
addrx_addr([], []).
addrx_addr([0|AddrXT], [0|AddrT]) :- addrx_addr(AddrXT, AddrT).
addrx_addr([1|AddrXT], [1|AddrT]) :- addrx_addr(AddrXT, AddrT).
addrx_addr([x|AddrXT], [AddrH|AddrT]) :-
    (AddrH is 0; AddrH is 1),
    addrx_addr(AddrXT, AddrT).

process_all(__Level, M, [], M).
process_all(Level, Machine, [H|T], NewMachine) :-
    format("process ~w~n", H),
    process(Level, Machine, H, IntermediateMachine),
    process_all(Level, IntermediateMachine, T, NewMachine).

binary_decimal(BinaryList, Decimal) :-
    length(BinaryList, 36),
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
    process_all(1, machine([], _), Data, machine(FinalMemory, _)),
    aggregate_all(sum(Val), (member(_-BinaryVal, FinalMemory), binary_decimal(BinaryVal, Val)), Result),
    !.

level(2, Data, Result) :-
    process_all(2, machine([], _), Data, machine(FinalMemory, _)),
    aggregate_all(sum(Val), (member(_-BinaryVal, FinalMemory), binary_decimal(BinaryVal, Val)), Result),
    !.

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

:-  set_prolog_flag(stack_limit, 8_000_000_000),
    profile(run(test, 1)),
    profile(run(real, 1)),
    profile(run(test, 2)),
    profile(run(real, 2)).