% using the dcg/basics library for simple "character class"-type DCGs.
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

%% --------
%% Data I/O
%% --------

% input format
input([Num]) --> integer(Num).
input([Num|Rest]) --> integer(Num), blanks, input(Rest).

% reads a list of numbers
read_numbers(Numbers) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/09_input.txt", FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(input(Numbers), FileTextCodes).

%% ----------
%% Predicates
%% ----------

% a position Pos is valid if
%   - Pos is within bounds (lower limit: preamble, upper limit: list length)
%   - there are two numbers N1 and N2 within the preamble (at position N1_Pos and N2_Pos respectivly) 
%   - the sum of N1 and N2 equals the number X at position Pos.
valid_position(Numbers, Preamble_Len, Pos) :-
    length(Numbers, ListLen),
    member_between0(X, Pos, Numbers, Preamble_Len, ListLen),
    Preamble_End is Pos - 1,                           % for Pos = 25, PreambleLen = 25: 24
    Preamble_Start is Preamble_End - Preamble_Len + 1, % for Pos = 25, PreambleLen = 25: 0
    member_between0(N1, N1_Pos, Numbers, Preamble_Start, Preamble_End),
    member_between0(N2, N2_Pos, Numbers, Preamble_Start, Preamble_End),
    N2_Pos > N1_Pos,
    X is N1 + N2.

invalid_position(Numbers, Preamble_Len, Pos) :- not(valid_position(Numbers, Preamble_Len, Pos)).

% utility predicate to access list members in a certain index range
% index is 0-based
member_between0(Member, Idx, List, Start, End) :-
    between(Start, End, Idx),
    nth0(Idx, List, Member).

%% -----
%% Level
%% -----

level(1, Numbers, Result) :-
    length(Numbers, ListLen),
    between(25, ListLen, Pos),
    invalid_position(Numbers, 25, Pos),
    nth0(Pos, Numbers, Result).

% find a list of numbers Group before the number Goal within the list Numbers such that
% the sum of the numbers in Group equals Goal.
% then, calculate the sum of the smallest and largest number within Group.
level(2, Numbers, Goal, Result) :-
    append([_, Group, _], Numbers),
    length(Group, GroupLength),
    GroupLength > 1,
    sum_list(Group, Goal),
    min_member(Min, Group),
    max_member(Max, Group),
    Result is Min + Max.

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
    level(2, Numbers, Res1, Res2), write('Level 2: '), writeln(Res2),
    statistics(runtime,[Stop2|_]),
    Runtime2 is Stop2 - Start2,
    write('Ran: '), write(Runtime2), writeln('ms').