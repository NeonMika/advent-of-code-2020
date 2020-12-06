%% --------
%% Data I/O
%% --------

% read from file
% returns a list of lists of strings, e.g., [[abc,abe],[a,b,c]]
% we call each list of strings a *Group*
read_groups(Groups) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/06_input.txt", FileText, []),
    atomic_list_concat(Blocks,'\n\n', FileText), % this can split multiline -> split into blocks
    maplist(build_input, Blocks, Groups), % split each block into list of strings
    !.
build_input(Line, Input) :-
    split_string(Line, '\n ', '', Input).

%% ----------
%% Predicates
%% ----------

% N equals the number of chars that appear in at least one of the strings in Group
distinct_chars(Group, N) :-
    atomic_list_concat(Group, ConcatedStrings),
    atom_chars(ConcatedStrings, AllChars),   
    sort(AllChars, DistinctChars), % also performs distinct
    length(DistinctChars, N).

% succeeds if X is between a and z
char(X) :- between(97, 122, ASCII), atom_char(X, ASCII).

% succeeds if Char is part of String
char_in_string(Char, String) :- char(Char), atom_chars(String, Chars), member(Char, Chars).

% succeeds if Char appears in every string of Group
char_in_every_string(Char, Group) :- char(Char), forall(member(String, Group), char_in_string(Char, String)).

% N equals the number of chars that appear in every strings in Group
shared_chars(Group, N) :-
    % remember to take care of situations where N = 0, i.e., do not use bagof because it fails if the result list is empty
    % Longer version: findall(char(C), char_in_every_string(C, Group), SharedChars), length(SharedChars, N).
    aggregate_all(count, (char(C), char_in_every_string(C, Group)), N).

%% ------
%% Levels
%% ------

level(1, Groups, Res) :-
    maplist(distinct_chars, Groups, DistinctCharsCounts),
    sum_list(DistinctCharsCounts, Res).

level(2, Groups, Res) :-
    maplist(shared_chars, Groups, SharedCharCounts),
    sum_list(SharedCharCounts, Res).

%% ----------------
%% Auto-run on load
%% ----------------

:-
    read_groups(Groups),
    level(1, Groups, Res1), write('Level 1: '), writeln(Res1), 
    level(2, Groups, Res2), write('Level 2: '), writeln(Res2).