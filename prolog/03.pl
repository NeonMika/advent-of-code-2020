%% --------
%% Data I/O
%% --------

% reads in a map and converts it to a list of lists of integers
read_map(Map, Width, Height) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/03_input.txt", FileText, []),
    split_string(FileText, "\n", "", Strings),
    maplist(build_input, Strings, Map),
    [FirstLine|_] = Map,
    length(FirstLine, Width),
    length(Map, Height),
    !.

build_input(Line, Input) :-
    %re_matchsub("(\\d*)-(\\d*) (.): (.*)", Line, Dict, []),
    string_chars(Line, LineChars),
    maplist(is_hash, LineChars, Input).

%% -----
%% Facts
%% -----

is_hash(#, 1).
is_hash(_, 0).

%% ----------
%% Predicates
%% ----------

% range(X, L, H, Step)
% X is between L and H (exclusive) and a multiple of Step offset from L
range(X, L, H, Step) :-
    X is L + Step,
    X < H.
range(X, L, H, Step) :-
    L1 is L + Step,
    L1 < H,
    range(X, L1, H, Step).

% succeeds if the given position is valid based on a given X_Offset and Y_Offset
slope_step(pos(Y, X, Tree), X_Offset, Y_Offset) :-
    read_map(Map, Width, Height),
    range(Y, 0, Height, Y_Offset),
    X is Y / Y_Offset * X_Offset mod Width,
    nth0(Y, Map, Line),
    nth0(X, Line, Tree).

% succeed if Hits matches the number of tree hits when using a certain X_Offset and Y_Offset
tree_hits(X_Offset, Y_Offset, Hits) :- aggregate_all(sum(Tree), slope_step(pos(_, _, Tree), X_Offset, Y_Offset), Hits).

% multiplication helper method for product foldl
mul(V1,V2,R) :- R is V1*V2.

% product
product(List, Res) :- foldl(mul, List, 1, Res).

%% ------
%% Levels
%% ------

level_1(Res) :- tree_hits(3, 1, Res).

level_2(Res) :-
    findall(X, level_2_part_solutions(X), Solutions),
    product(Solutions, Res).

level_2_part(1, 1).
level_2_part(3, 1).
level_2_part(5, 1).
level_2_part(7, 1).
level_2_part(1, 2).

level_2_part_solutions(Res) :-
    level_2_part(X_Offset, Y_Offset),
    tree_hits(X_Offset, Y_Offset, Res).

%% ----------------
%% Auto-run on load
%% ----------------

:- 
    level_1(Res1), write('Level 1: '), writeln(Res1), 
    level_2(Res2), write('Level 2: '), writeln(Res2).