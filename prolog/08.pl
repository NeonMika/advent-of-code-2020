% using the dcg/basics library for simple "character class"-type DCGs.
:- use_module(library(dcg/basics)).

%% --------
%% Data I/O
%% --------

% input format
code([Op]) --> line(Op).
code([Op|Rest]) --> line(Op), blanks, code(Rest).
line(nop(I)) --> "nop ", integer(I).
line(acc(I)) --> "acc ", integer(I).
line(jmp(I)) --> "jmp ", integer(I).

% reads a list of operations
read_code(Code) :-
    working_directory(_, "C:/repos/git/advent-of-code-2020"),
    read_file_to_string("data/08_input.txt", FileText, []),
    string_codes(FileText, FileTextCodes),
    phrase(code(Code), FileTextCodes).

%% ----------
%% Predicates
%% ----------

% final step reached if we visit a PC outside of code range -> set success to false and cut!
process(_, state(Acc, PC, VisitedPC, _), state(Acc, PC, VisitedPC, false)) :- 
    PC < 0, 
    !.
% final step reached if we visit a PC outside of code range -> set success to false and cut!
process(Code, state(Acc, PC, VisitedPC, _), state(Acc, PC, VisitedPC, false)) :- 
    length(Code, CodeLength), 
    PC > CodeLength, 
    !.
% final step reached if we visit a certain PC again -> set success to false and cut!
process(_, state(Acc, PC, VisitedPC, _), state(Acc, PC, VisitedPC, false)) :- 
    member(PC, VisitedPC), 
    !.
% final step reached if we want to visit PC after last code segment -> set success to true and cut!
process(Code, state(Acc, PC, VisitedPC, _), state(Acc, PC, VisitedPC, true)) :- 
    length(Code, CodeLength),
    PC = CodeLength,
    !.
% process nop
process(Code, state(Acc, PC, VisitedPC, _), FinalState) :-
    nth0(PC, Code, nop(_)),
    NewPC is PC + 1, 
    process(Code, state(Acc, NewPC, [PC | VisitedPC], _), FinalState).
% process acc
process(Code, state(Acc, PC, VisitedPC, _), FinalState) :-
    nth0(PC, Code, acc(Val)),
    NewPC is PC + 1, 
    NewAcc is Acc + Val, 
    process(Code, state(NewAcc, NewPC, [PC | VisitedPC], _), FinalState).
% process jmp
process(Code, state(Acc, PC, VisitedPC, _), FinalState) :- 
    nth0(PC, Code, jmp(Val)),
    NewPC is PC + Val, 
    process(Code, state(Acc, NewPC, [PC | VisitedPC], _), FinalState).

%% -----
%% Level
%% -----

level(1, Code, Result) :- 
    process(Code, state(0,0,[],_), state(Result,_,_,_)). 

level(2, Code, Result) :-
    select(nop(X), Code, jmp(X), ModifiedCode),
    process(ModifiedCode, state(0,0,[],false), state(Result,_,_,Success)),
    Success = true.

level(2, Code, Result) :-
    select(jmp(X), Code, nop(X), ModifiedCode),
    process(ModifiedCode, state(0,0,[],false), state(Result,_,_,Success)),
    Success = true.

%% ----------------
%% Auto-run on load
%% ----------------

:-
    read_code(Code),
    statistics(runtime,[Start1|_]),
    level(1, Code, Res1), write('Level 1: '), writeln(Res1),
    statistics(runtime,[Stop1|_]),
    Runtime1 is Stop1 - Start1,
    write('Ran: '), write(Runtime1), writeln('ms'),
    statistics(runtime,[Start2|_]),
    level(2, Code, Res2), write('Level 2: '), writeln(Res2),
    statistics(runtime,[Stop2|_]),
    Runtime2 is Stop2 - Start2,
    write('Ran: '), write(Runtime2), writeln('ms').