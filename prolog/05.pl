use_module(library(pcre)).
%split_string(String, "\n", "", Inputs), % this cannot split multiline, 2nd parameter is "string of sep chars", not "seperator string"
%atomic_list_concat(Blocks,'\n\n',String), % this can split multiline

read_it(Inputs) :-
	working_directory(_, 'C:/repos/git/advent-of-code-2020/prolog'),
    read_file_to_string('05_input.txt', FileText, []),
	split_string(FileText, '\n', '', Lines),
    maplist(build_input, Lines, Inputs),
    !.

build_input(Line, seat(R_B, C_B, R, C, ID)) :- 
	string_chars(Line, [R1, R2, R3, R4, R5, R6, R7, C1, C2, C3]),
	R_B = [R1, R2, R3, R4, R5, R6, R7],
	C_B = [C1, C2, C3],
	row_pos(R_B, R),
	col_pos(C_B, C),
	seat_id(R, C, ID).

binary_string_to_pos(X-X, _, _, X) :- 
	% Final step, Low = High = Result
	!.
binary_string_to_pos(Low-High, [Sym | T], Sym, Result) :-
	% Bottom half
	!,
	NewHigh is truncate((Low + High) / 2),
	binary_string_to_pos(Low-NewHigh, T, Sym, Result).
binary_string_to_pos(Low-High, [_ | T], Sym, Result) :-
	% Top half
	NewLow is truncate((Low + High + 1) / 2),
	binary_string_to_pos(NewLow-High, T, Sym, Result).

row_pos(R_B, R) :- binary_string_to_pos(0-127, R_B, 'F', R).

col_pos(C_B, C) :- binary_string_to_pos(0-7, C_B, 'L', C).

seat_id(R, C, ID) :-  ID is R * 8 + C.

extract_id(seat(_,_,_,_,ID), ID).

first_unconsecutive_elements([H1|T], H1, H2) :-
	T = [H2|_],
	Inc is H1 + 1,
	H2 \= Inc,
	!.
first_unconsecutive_elements([_|T], X, Y) :- first_unconsecutive_elements(T, X, Y).

level(1, Seats, Result) :- aggregate_all(max(ID), member(seat(_,_,_,_,ID), Seats), Result).
level(2, Seats, Result) :-
	maplist(extract_id, Seats, Ids),
	sort(Ids, SortedIds),
	first_unconsecutive_elements(SortedIds, X, _),
	Result is X + 1.

:- 
	read_it(Seats),
	level(1, Seats, Res1), 
	write(Res1), write('\n'),
	level(2, Seats, Res2),
	write(Res2), write('\n').