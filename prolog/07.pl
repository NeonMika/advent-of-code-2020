%% --------
%% Data I/O
%% --------

% read from file
% returns a list of node structures
read_nodes() :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/07_input.txt", FileText, []),
    atomic_list_concat(Lines,"\n", FileText),
    exclude(
        {}/[Line]>>(
            re_matchsub("(.*) bags contain (.*)\\.", Line, re_match{0:_,1:Name,2:RefInfo}, []),
            atom_string(Name_Atom, Name),
            assertz(node(Name_Atom)),
            RefInfo \= "no other bags",
            atomic_list_concat(Parts,", ", RefInfo),
            exclude(
                {}/[Part]>>(
                    re_matchsub("(\\d+) (.*) (?:bag|bags)", Part, re_match{0:_,1:N,2:Ref}, []),
                    atom_number(N, N_I),
                    atom_string(Ref_Atom, Ref), 
                    foreach(between(1, N_I, _), assertz(points(Name_Atom, Ref_Atom)))
                ), 
                Parts, 
                _
            )
        ), 
        Lines, 
        _
    ).

%% ----------
%% Predicates
%% ----------
pointed(X, Y) :- points(Y, X).

pointedI(X, Y) :- pointed(X, Y).
pointedI(X, Y) :- pointed(X, Z), pointedI(Z, Y).

pointsI(X, Y) :- points(X, Y).
pointsI(X, Y) :- points(X, Z), pointsI(Z, Y).

%% ------
%% Levels
%% ------

level(1, Result) :- 
    findall(X, pointedI('shiny gold', X), Ancestors), 
    sort(Ancestors, DistinctAncestors), 
    length(DistinctAncestors, Result).

level(2, Result) :-
    findall(X, pointsI('shiny gold', X), Descendants), 
    length(Descendants, Result).

%% ----------------
%% Auto-run on load
%% ----------------

:-
    read_nodes(),
    level(1, Res1), write('Level 1: '), writeln(Res1),
    level(2, Res2), write('Level 2: '), writeln(Res2).