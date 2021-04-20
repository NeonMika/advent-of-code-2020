:- use_module(library(clpfd)).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

level(13).

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

% Taken from RosettaCode - https://rosettacode.org/wiki/Chinese_remainder_theorem#Prolog

product(A, B, C) :- C is A*B.
 
pair(X, Y, X-Y).
 
egcd(_, 0, 1, 0) :- !.
egcd(A, B, X, Y) :-
    divmod(A, B, Q, R),
    egcd(B, R, S, X),
    Y is S - Q*X.
 
modinv(A, B, X) :-
    egcd(A, B, X, Y),
    A*X + B*Y =:= 1.
 
crt_fold(A, M, P, R0, R1) :- % system of equations of (x = a) (mod m); p = M/m
    modinv(P, M, Inv),
    R1 is R0 + A*Inv*P.
 
crt(Pairs, N) :-
    maplist(pair, As, Ms, Pairs),
    foldl(product, Ms, 1, M),
    maplist(divmod(M), Ms, Ps, _), % p(n) <- M/m(n)
    foldl(crt_fold, As, Ms, Ps, 0, N0),
    N is N0 mod M.

%% ------------------
%% Input
%% ------------------

input(data(EarliestStart, Lines)) --> 
    integer(EarliestStart), 
    blanks, 
    sequence(string, ",", LineCodes), 
    { convlist([Codes, Result]>>(string_codes(String, Codes), (number_string(Result, String) -> Result = Result; Result = x)), LineCodes, Lines) }.

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

earliest_line_departure(EarliestStart, Line, Dep) :-
    Dep in 1..9999999999999,
    Dep #>= EarliestStart,
    Dep #= _ * Line,
    labeling([min(Dep)], [Dep]), !.    

%% -----
%% Level
%% -----

level(1, Data, Result) :-
    Data = data(EarliestStart, Lines),
    aggregate_all(min(LineDep, Line), (member(Line, Lines), Line \= x, earliest_line_departure(EarliestStart, Line, LineDep)), min(Dep, Line)),   
    Result #= Line * (Dep - EarliestStart).

level(2, Data, Result) :-
    Data = data(_, Lines),    
    findall(-Index-Line, (nth0(Index, Lines, Line), Line \= x), Pairs),
    crt(Pairs, T),
    Result = T.

%% ----------------
%% Auto-run on load
%% ----------------

:-  level(Level),
    writeln("Testdata:"),
    read_input(TestData, true, Level),
    statistics(runtime,[TestStart1|_]),
    level(1, TestData, TestResult1), 
    statistics(runtime,[TestStop1|_]),
    TestRuntime1 is TestStop1 - TestStart1,
    format("> Level 1:~n>> Result: ~w~n>> (ran for ~wms)~n", [TestResult1, TestRuntime1]),
    statistics(runtime,[TestStart2|_]),
    level(2, TestData, TestResult2), 
    statistics(runtime,[TestStop2|_]),
    TestRuntime2 is TestStop2 - TestStart2,
    format("> Level 2:~n>> Result: ~w~n>> (ran for ~wms)~n~n", [TestResult2, TestRuntime2]),

    writeln("Real data:"),
    read_input(Data, false, Level),
    statistics(runtime,[Start1|_]),
    level(1, Data, Result1),
    statistics(runtime,[Stop1|_]),
    Runtime1 is Stop1 - Start1,
    format("> Level 1:~n>> Result: ~w~n>> (ran for ~wms)~n", [Result1, Runtime1]),
    statistics(runtime,[Start2|_]),
    level(2, Data, Result2),
    statistics(runtime,[Stop2|_]),
    Runtime2 is Stop2 - Start2,
    format("> Level 2:~n>> Result: ~w~n>> (ran for ~wms)~n", [Result2, Runtime2]).