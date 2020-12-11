:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(gv)).

:- dynamic(mem_descent/2).

%% --------
%% Data I/O
%% --------

% input format
input([Num]) --> integer(Num).
input([Num|Rest]) --> integer(Num), blanks, input(Rest).

% reads a list of numbers
read_numbers(Numbers) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/10_input.txt", FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(input(UnsortedNumbers), FileTextCodes),
    max_list(UnsortedNumbers, MaxAdapter),
    DeviceJolt is MaxAdapter + 3,
    sort([0,DeviceJolt|UnsortedNumbers], Numbers).

%% ----------
%% Predicates
%% ----------

neighbor_differences(List, DiffList) :- 
    findall(Diff, (nextto(A, B, List), Diff is B-A), DiffList).

descent([],   0) :- !.
descent([_],  1) :- !.
descent(List, R) :-
    findall(Rest, (append([A | _], Rest, List), Rest = [B | _], Diff is B - A, Diff =< 3), DescentLists),
    foldl(accumulate_descent, DescentLists, 0, R).

accumulate_descent(List, Acc, Res) :- 
    mem_descent(List, R), % take from cache
    Res is Acc + R.
accumulate_descent(List, Acc, Res) :-
    descent(List, R),
    assertz(mem_descent(List, R)), % put into cache
    Res is Acc + R.
    

%% -----
%% Level
%% -----

level(1, Numbers, Result) :-
    neighbor_differences(Numbers, Diffs),
    aggregate_all(count, member(1, Diffs), OneDiff),
    aggregate_all(count, member(3, Diffs), ThreeDiff),
    Result is OneDiff * ThreeDiff.

level(2, Numbers, Result) :-
    descent(Numbers, Result).

%% ----------------
%% Auto-run on load
%% ----------------

:-
    read_numbers(Numbers),
    statistics(runtime,[Start1|_]),
    level(1, Numbers, Res1), write('Level 1: '), writeln(Res1),
    statistics(runtime,[Stop1|_]),
    Runtime1 is Stop1 - Start1,
    write('Ran: '), write(Runtime1), writeln('ms'),
    statistics(runtime,[Start2|_]),
    level(2, Numbers, Res2), write('Level 2: '), writeln(Res2),
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