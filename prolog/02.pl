use_module(library(pcre)).

read_it(Stuff) :-
    read_file_to_string('02_input.txt', String, []),
    split_string(String, "\n", "", Strings),
    maplist(build_inputs, Strings, Stuff).

build_inputs(Line, Inputs) :-
    re_matchsub("(\\d*)-(\\d*) (.): (.*)", Line, Dict, []),
    number_string(N1, Dict.1),
    number_string(N2, Dict.2),
    atom_string(Needle, Dict.3),
    string(Haystack) = string(Dict.4),
    Inputs = input(N1, N2, Needle, Haystack).

level_1(Count) :-
    aggregate_all(count, internal_1, Count).

internal_1() :-
    read_it(Inputs),
    member(Input, Inputs),
    Input = input(Min, Max, Needle, Haystack),
    string_chars(Haystack, HaystackChars),
    count(Needle, HaystackChars, N),
    N >= Min,
    N =< Max.

level_2(Count) :-
    aggregate_all(count, internal_2, Count).

internal_2() :-
    read_it(Inputs),
    member(Input, Inputs),
    Input = input(Left_i, Right_i, Needle, Haystack),
    string_chars(Haystack, HaystackChars),
    nth1(Left_i, HaystackChars, Left),
    nth1(Right_i, HaystackChars, Right),
    (Left = Needle; Right = Needle),
    Left \= Right.

count(_, [], 0).
count(Num, [H|T], X) :- Num \= H, count(Num, T, X).
count(Num, [H|T], X) :- Num = H, count(Num, T, X1), X is X1 + 1.





