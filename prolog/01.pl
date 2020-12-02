% not needed due to auto-import
% use_module(library(readutil)).

read_numbers(Numbers) :-
    read_file_to_string('01_input.txt', String, []),
    split_string(String, "\n", "", Strings),
    maplist(atom_number, Strings, Numbers).

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



