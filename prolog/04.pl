%% --------
%% Data I/O
%% --------

% reads a list of lists of strings
% each string represents a property of a passport in the format "key:value"
read_passports(Passports) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/04_input.txt", FileText, []),
    atomic_list_concat(Blocks,'\n\n',FileText), % this can split multiline
    maplist(build_input, Blocks, Passports),
    !.

build_input(Line, Input) :-
    split_string(Line, '\n ', '', Input). % split each block at space and newline

%% ----------
%% Predicates
%% ----------

% succeeds if Passport contains all needed properties for level 1
valid_simple_passport(Passport) :-
    Props = [byr, iyr, eyr, hgt, hcl, ecl, pid],
    include(passport_property(Passport), Props, Props).

% succeeds if Passport contains a property that starts with the three letters Prop
passport_property(Passport, Prop) :-
    member(X, Passport),
    sub_string(X, 0, 3, _, Prop).

% succeeds if Passport contains all needed properties and matches the restrictions of level 2
valid_detailed_passport(Passport) :-    
	member(ByrMember, Passport),
	re_matchsub("byr:(\\d{4})(?:\\D|$)", ByrMember, re_match{0:_,1:Byr}, []),
	atom_number(Byr, ByrNumber),
	ByrNumber >= 1920,
	ByrNumber =< 2002,
	
	member(IyrMember, Passport),
	re_matchsub("iyr:(\\d{4})(?:\\D|$)", IyrMember, re_match{0:_,1:Iyr}, []),
	atom_number(Iyr, IyrNumber),
	IyrNumber >= 2010,
	IyrNumber =< 2020,
	
	member(EyrMember, Passport),
	re_matchsub("eyr:(\\d{4})(?:\\D|$)", EyrMember, re_match{0:_,1:Eyr}, []),
	atom_number(Eyr, EyrNumber),
	EyrNumber >= 2020,
	EyrNumber =< 2030,
	
	member(HgtMember, Passport),
	re_matchsub("hgt:((?:59|6\\d|7[0-6])in|1(?:[5-8]\\d|9[0-3])cm)", HgtMember, _, []),
	
	member(HclMember, Passport),
	re_matchsub("hcl:(#[0-9a-f]{6})", HclMember, _, []),
	
	member(EclMember, Passport),
	re_matchsub("ecl:(amb|blu|brn|gry|grn|hzl|oth)", EclMember, _, []),
	
	member(PidMember, Passport),
	re_matchsub("pid:(\\d{9})(?:\\D|$)", PidMember, _, []).

%% ------
%% Levels
%% ------

level_1(Res) :-
	read_passports(Passports),
	aggregate_all(count, (member(Passport, Passports), valid_simple_passport(Passport)), Res).

level_2(Res) :-
	read_passports(Passports),
	aggregate_all(count, (member(Passport, Passports), valid_detailed_passport(Passport)), Res).

%% ----------------
%% Auto-run on load
%% ----------------

:- 
    level_1(Res1), write('Level 1: '), writeln(Res1), 
    level_2(Res2), write('Level 2: '), writeln(Res2).