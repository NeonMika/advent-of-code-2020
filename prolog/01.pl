%% --------
%% Data I/O
%% --------

% reads in a list of numbers from file
read_numbers(Numbers) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/01_input.txt", FileText, []),
    split_string(FileText, "\n", "", Strings),
    maplist(atom_number, Strings, Numbers).

%% ------
%% Levels
%% ------

level_1(Mul) :-
    read_numbers(Numbers),
    nth0(X_i, Numbers, X),
    nth0(Y_i, Numbers, Y),
    X_i < Y_i,
    2020 is X + Y,
    Mul is X * Y.

level_2(Mul) :-
    read_numbers(Numbers),
    nth0(X_i, Numbers, X),
    nth0(Y_i, Numbers, Y),
    nth0(Z_i, Numbers, Z),
    X_i < Y_i,
    Y_i < Z_i,
    2020 is X + Y + Z,
    Mul is X * Y * Z.

%% ----------------
%% Auto-run on load
%% ----------------

:- 
    level_1(Res1), write('Level 1: '), writeln(Res1), 
    level_2(Res2), write('Level 2: '), writeln(Res2).