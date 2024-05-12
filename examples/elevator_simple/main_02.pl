/**
    Main file for running the simple Elevator examples.

    This file loads the interpreter and the application file, and has a main/1 predicate
    to run the available controllers.
**/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% top-level interpreter
:- interpreter(indigolog_plain, F), consult(F).
% :- ['../../Interpreters/indigolog_plain_swi'].

% 4 - Consult application
:- [elevator_02].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets IndiGolog to evaluate a chosen mainControl procedure
main :-
    findall(C, proc(control(C), _), L),
    repeat,
    format('Controllers available: ~w\n', [L]),
    write('Select controller: '),
	read(S), nl,
    member(S, L),
	format('Executing controller: *~w*\n', [S]), !,
    indigolog(control(S)).

main(C) :- 	indigolog(control(C)).


