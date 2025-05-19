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

:- ensure_loaded('transfinal-ext').  	% extended constructs
:- ensure_loaded('transfinal-search').  % search constructs (INDIGOLOG)

% basic trans/final from ConGolog (includes Golog) - must be loaded last!
:- ensure_loaded('transfinal-congolog').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTHER TOOLS ON TOP OF TRANS/FINAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Golog-like do/3

%% !do(+E, +H0, -H) is ndet.
%
%  Program E execute in full from H0 to H
%
%  @param E   	ConGolog program to execute
%  @param H0  	Initial history/situation
%  @param H  	Final history/situation
do(E, H) :- do(E, [], H).
% uncomment if you want to debug histories considered
% do(_, [A|H], _) :- exog_action(A), writeln([A|H]), fail.
% do(_, [A|H], _) :-
% 	A = withdrawal_by_applicant(_),
% 	writeln([A|H]),
% 	fail.
do(E, H, H) :- final(E, H).
do(E, H, H2) :- trans(E, H, E1, H1), do(E1, H1, H2).


%% !trans_star(+E, +H, -E1, ?H1) is ndet.
%
%  Transitive closure of trans/4
%  Configuration (E, H) can evolve to configuration (E1, H1)
trans_star(E, H, E, H).
trans_star(E, H, E1, H1) :-
	trans(E, H, E2, H2),
	% before/1: from evaluator; checks H2 is subhistory of H1
	(var(H1) -> true ; once(before(H2, H1))),
    trans_star(E2, H2, E1, H1).
final_star(E, H) :- final(E, H).
final_star(E, H) :- trans_star(E, H, E2, H), E2 \= E, final_star(E2, H).

%% !trans_star(+E, +H, -E1, ?H1, +N) is ndet.
%
%  Transitive closure of trans/4 for N steps
%  Configuration (E, H) can evolve to configuration (E1, H1) in N steps
trans_star(E, H, E, H, 0) :- !.
trans_star(E, H, EN, HN, N) :-
	trans(E, H, E1, H1),
	(var(HN) -> true ; once(before(H1, HN))),
	N2 is N-1,
	trans_star(E1, H1, EN, HN, N2).


%% !trans_block(+E, +H, -E1, ?H1) is ndet.
%
%  Transitive closure of trans/4  up to a blocked configuration (E1, H1)
trans_block(E, H, E1, H1) :-
	trans_star(E, H, E1, H1), \+ trans(E1, H1, _, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%