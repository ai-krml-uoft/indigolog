%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Top-level Transition System for INDIGOLOG

    Part of the INDIGOLOG system

    Refer to root directory for license, documentation, and information
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                            TRANS and FINAL
%% Trans(E, H, E1, H1) ->  One execution step of program E from history H
%%			 leads to program E1 with history H1.
%% Final(E, H)       ->  Program E at history H is final.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile(trans/4),
   multifile(final/2).


:- ensure_loaded('transfinal-ext').  	% Load extended constructs
:- ensure_loaded('transfinal-search').  % Load search constructs (INDIGOLOG)

% basic trans/final from ConGolog (includes Golog) - must be loaded last!
:- ensure_loaded('transfinal-congolog').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTHER TS TOOLS ON TOP OF TRANS/FINAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Added for KR course....(Golog-like do/3)
% do(E, H, H3) :-
% 	trans(search(E), H, E2, H2),
% 	trans_star(E2, H2, E3, H3),
% 	final(E3, H3).

do(E, H) :- do(E, [], H).

% do(_, [A|_], _) :- exog_action(A), writeln(A), fail.
% do(_, [A|H], _) :-
% 	A = withdrawal_by_applicant(_),
% 	writeln([A|H]),
% 	fail.

do(E, H, H) :- final(E, H).
do(E, H, H2) :- trans(E, H, E1, H1), do(E1, H1, H2).



% Transitive clousure of trans/4
trans_star(E, H, E, H).
trans_star(E, H, E1, H1) :-
	trans(E, H, E2, H2),
	% before/1: from evaluator; checks H2 is subhistory of H1
	(var(H1) -> true ; once(before(H2, H1))),
    trans_star(E2, H2, E1, H1).

% transitive version of final/2 combined
final_star(E, H) :- final(E, H).
final_star(E, H) :- trans_star(E, H, E2, H), E2 \= E, final_star(E2, H).

% trans/5 performs a defined number N of consecutive trans steps
trans_star(E, H, E, H, 0) :- !.
trans_star(E, H, EN, HN, N) :-
	trans(E, H, E1, H1),
	(var(H1) -> true ; once(before(H1, HN))),
	N2 is N-1,
	trans_star(E1, H1, EN, HN, N2).


% Stores a node/4 entry in DB with Id and program E and history H
store_node(Id, E, H) :-
	(retract(counter(N)) -> N2 is N+1 ; N2 = 1),
	assert(node(Id, N2, E, H)),
	assert(counter(N2)).

% Evolve (E, H) as much as possible (i.e., until it blocks)
trans_block(E, H, E2, H2) :- trans_star(E, H, E2, H2), \+ trans(E2, H2, _, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%