use_module(library(pcre)).

read_it(Inputs) :-
    read_file_to_string('04_input.txt', String, []),
    %split_string(String, "\n\n", "", Inputs), % this cannot split multiline, since 2nd parameter takes "list / string of sep chars", not "seperator string"
    atomic_list_concat(Blocks,'\n\n',String), % this can split multiline
    maplist(build_input, Blocks, Inputs),
    !.

build_input(Line, Input) :-
    split_string(Line, '\n ', '', Input). % split each block at space and newline

% call with aggregate_all(count, level_1(X), Sum).
level_1(Block) :-
    read_it(Blocks),
    member(Block, Blocks),
    valid_simple_block(Block).

valid_simple_block(Block) :-
    Props = [byr, iyr, eyr, hgt, hcl, ecl, pid],
    include(has_property(Block), Props, Props).

has_property(Block, Prop) :-
    member(X, Block),
    sub_string(X, 0, 3, _, Prop).

% call with aggregate_all(count, level_2(X), Sum).
level_2(Passport) :-
    read_it(Blocks),
    member(Block, Blocks),
    valid_detailed_block(Block, Passport).

valid_detailed_block(Block, Passport) :-    
	member(ByrMember, Block),
	re_matchsub("byr:(\\d{4})(?:\\D|$)", ByrMember, re_match{0:_,1:Byr}, []),
	atom_number(Byr, ByrNumber),
	ByrNumber >= 1920,
	ByrNumber =< 2002,
	
	member(IyrMember, Block),
	re_matchsub("iyr:(\\d{4})(?:\\D|$)", IyrMember, re_match{0:_,1:Iyr}, []),
	atom_number(Iyr, IyrNumber),
	IyrNumber >= 2010,
	IyrNumber =< 2020,
	
	member(EyrMember, Block),
	re_matchsub("eyr:(\\d{4})(?:\\D|$)", EyrMember, re_match{0:_,1:Eyr}, []),
	atom_number(Eyr, EyrNumber),
	EyrNumber >= 2020,
	EyrNumber =< 2030,
	
	member(HgtMember, Block),
	re_matchsub("hgt:((?:59|6\\d|7[0-6])in|1(?:[5-8]\\d|9[0-3])cm)", HgtMember, re_match{0:_,1:Hgt}, []),
	
	member(HclMember, Block),
	re_matchsub("hcl:(#[0-9a-f]{6})", HclMember, re_match{0:_,1:Hcl}, []),
	
	member(EclMember, Block),
	re_matchsub("ecl:(amb|blu|brn|gry|grn|hzl|oth)", EclMember, re_match{0:_,1:Ecl}, []),
	
	member(PidMember, Block),
	re_matchsub("pid:(\\d{9})(?:\\D|$)", PidMember, re_match{0:_,1:Pid}, []),
	
	Passport = passport(ByrNumber, IyrNumber, EyrNumber, Hgt, Hcl, Ecl, Pid).