use_module(library(pcre)).

read_it(Inputs, Width, Height) :-
    read_file_to_string('03_input.txt', String, []),
    split_string(String, "\n", "", Strings),
    maplist(build_input, Strings, Inputs),
    [FirstLine|_] = Inputs,
    length(FirstLine, Width),
    length(Inputs, Height),
    !.

build_input(Line, Input) :-
    %re_matchsub("(\\d*)-(\\d*) (.): (.*)", Line, Dict, []),
    string_chars(Line, LineChars),
    maplist(is_hash, LineChars, Input).

is_hash(#, 1).
is_hash(_, 0).

slope_step(pos(Y, X, Tree), X_Offset, Y_Offset) :-
    read_it(Map, Width, Height),
    range(Y, 0, Height, Y_Offset),
    X is Y / Y_Offset * X_Offset mod Width,
    nth0(Y, Map, Line),
    nth0(X, Line, Tree).

tree_hits(X_Offset, Y_Offset, Hits) :- aggregate_all(sum(Tree), slope_step(pos(_, _, Tree), X_Offset, Y_Offset), Hits).

range(X, L, H, Step) :-
    X is L + Step,
    X < H.
range(X, L, H, Step) :-
    L1 is L + Step,
    L1 < H,
    range(X, L1, H, Step).

mul(V1,V2,R) :- R is V1*V2.

level_1(Res) :- tree_hits(3, 1, Res).

level_2(Res) :-
    findall(X, level_2_part_solutions(X), Solutions),
    write(Solutions),
    foldl(mul, Solutions, 1, Res).

level_2_part(1, 1).
level_2_part(3, 1).
level_2_part(5, 1).
level_2_part(7, 1).
level_2_part(1, 2).

level_2_part_solutions(Res) :-
    level_2_part(X_Offset, Y_Offset),
    tree_hits(X_Offset, Y_Offset, Res).







