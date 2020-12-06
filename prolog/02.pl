%% --------
%% Data I/O
%% --------

% reads a list of password definitions from file
read_definitions(Definitions) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/02_input.txt", FileText, []),
    split_string(FileText, "\n", "", Strings),
    maplist(build_inputs, Strings, Definitions).

% maps each line to an input/4 structure 
build_inputs(Line, Inputs) :-
    re_matchsub("(\\d*)-(\\d*) (.): (.*)", Line, Dict, []),
    number_string(N1, Dict.1),
    number_string(N2, Dict.2),
    atom_string(Needle, Dict.3),
    string(Haystack) = string(Dict.4),
    Inputs = input(N1, N2, Needle, Haystack).

%% ----------
%% Predicates
%% ----------

% count_occurence(Num, List, X)
% X equals the number how often Num is contained in List
count_occurence(_, [], 0).
count_occurence(Num, [H|T], X) :- Num \= H, count_occurence(Num, T, X).
count_occurence(Num, [H|T], X) :- Num = H, count_occurence(Num, T, X1), X is X1 + 1.

%% ------
%% Levels
%% ------

level_1(Count) :-
    aggregate_all(count, internal_1, Count).

internal_1() :-
    read_definitions(Definitions),
    member(input(Min, Max, Needle, Haystack), Definitions),
    string_chars(Haystack, HaystackChars),
    count_occurence(Needle, HaystackChars, N),
    N >= Min,
    N =< Max.

level_2(Count) :-
    aggregate_all(count, internal_2, Count).

internal_2() :-
    read_definitions(Definitions),
    member(input(Left_i, Right_i, Needle, Haystack), Definitions),
    string_chars(Haystack, HaystackChars),
    nth1(Left_i, HaystackChars, Left),
    nth1(Right_i, HaystackChars, Right),
    (Left = Needle; Right = Needle),
    Left \= Right.

%% ----------------
%% Auto-run on load
%% ----------------

:- 
    level_1(Res1), write('Level 1: '), writeln(Res1), 
    level_2(Res2), write('Level 2: '), writeln(Res2).



