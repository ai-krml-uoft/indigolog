%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/transfinal.pl
%
%       IndiGolog TRANS & FINAL Implementation (Version 5)
%
%  AUTHOR : Sebastian Sardina
%           based on the definitions for ConGolog by
%			Giuseppe De Giaccomo, Yves Lesperance, and Hector Levesque
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%    This file contains the definition of TRANS and FINAL for all the
%	constructs in the language
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file provides:
%
% -- mfinal(E, H)		 : meta-version of final/2
% -- mtrans(E, H, E2, H2)	 : meta-version of trans/4
% -- trans(P, H, P2, H2)    : configuration (P, H) can perform a single step
%                          to configuration (P2, H2)
% -- final(P, H)          : configuration (P, H) is terminating
%
% -- do(P, H, H')          : Golog Do/3 using search(E)
%
%  The following special features are also provided:
%
% -- A special action `wait' that is always possible. It can be used to state
%         that the program will be waiting for an exogenous action.
% -- A special action `abort' that is always possible. It can be used to state
%         that the program should fail.
% -- A special action `sim(E)' for each exogenous action E. The action is
%         always possible and it is used to assume the occurrence of E
% -- A special action `stop_interrupts' that is used to set the fluent
%         interrupts_running to false
% -- A special fluent `interrupts_running' that is always true unless stopped
%
%
%  The following is required for this file:
%
% FROM SYSTEM CODE DEPENDING ON WHERE IT IS USED (hookvir.pl or hookrxc.pl)
% -- unknown(P, H): TRANS or FINAL for (P, H) is unknown
%                  (some condition is unknown to be true or false)
% -- report_message(T, M) : report message M of type T
%
% FROM TEMPORAL PROJECTOR:
% -- eval(+C, +H, -B)
%           B is the truth value of C at history H
% -- calc_arg(+A, -A2, +H)
%           calculate the arguments of action A at history H
% -- domain(-V, +D)
% -- rdomain(-V, +D)
%           object V is an element of domain D (random)
% -- getdomain(+D, -L)
%           L is the list of elements in domain D
% -- sensed(+A, ?V, ?H)
%           action A got sensing result V w.r.t. history H
% -- inconsistent(H)
%           last action make history H inconsistent, i.e. impossible
% -- assume(F, V, H1, H2)
%           H2 is the history resulting from assuming fluent F
%           to have value V at history H1
% -- before(+H1, +H2)
%           history H1 is a prefix of H2
%
% FROM DOMAIN SPECIFIC CODE:
% -- prim_action(action) : for each primitive action
% -- exog_action(action) : for each exogenous action
% -- poss(action, cond)   : precondition axioms
%
%  Code for describing the high-level program:
% -- proc(name, P)           : for each procedure P
% -- simulator(Id, C, A)      : Under simulator Id, exog action A must happens if C holds
%
% OTHERS (PROLOG SPECIFIC):
% -- false
%            equivalent to fail
% -- random(+L, +U, -R)
%            randomly returns a number R between L and U
% -- subv(+X1, +X2, +T1, -T2)
%            T2 is T1 with X1 replaced by X2
% -- catch/3 and throw/1 for handling exceptions
%            (provide empty implementations of both if there is no support
%            for exceptions available)
% -- shuffle/2 : shuffle a list into another list in a random way
%
% -- call_with_time_limit(+Sec, +Goal): True if Goal completes within Time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                            TRANS and FINAL
%% Trans(E, H, E1, H1) ->  One execution step of program E from history H
%%			 leads to program E1 with history H1.
%% Final(E, H)       ->  Program E at history H is final.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile(trans/4),
   multifile(final/2).


:- ensure_loaded('transfinal-ext').  	% Load extended constructs
:- ensure_loaded('transfinal-search').  % Load search constructs (IndiGolog)
:- ensure_loaded('transfinal-bdi').  	% Load BDI extensions (Yves Lesperance)

% basic trans/final from ConGolog (includes Golog) - must be loaded last!
:- ensure_loaded('transfinal-congolog').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTHER TS TOOLS ON TOP OF TRANS/FINAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Added for KR course....(Golog-like do/3)
do(E, H, H3) :-
	trans(search(E), H, E2, H2),
	trans_star(E2, H2, E3, H3),
	final(E3, H3).

% Transitive clousure of trans/4
trans_star(E, H, E, H).
trans_star(E, H, E1, H1) :-
	trans(E, H, E2, H2),
	(var(H1) ->
		true 			% always succ if var(H1)
	;
		once(before(H2, H1))	% If H1 is given, H2 is a subhistory of H1
	), 				% Avoid infinite trans_star steps
    trans_star(E2, H2, E1, H1).

% transitive version of final/2 combined
final_star(E, H) :- final(E, H).
final_star(E, H) :- trans_star(E, H, E2, H), E2 \= E, final_star(E2, H).

% trans/5 performs a defined number N of consequitives trans steps
trans(E, H, E, H, 0) :- !.
trans(E, H, E1, H1, N) :-
	N2 is N-1,
	trans(E, H, E2, H2, N2),
	trans(E2, H2, E1, H1).


% Stores a node/4 entry in DB with Id and program E and history H
store_node(Id, E, H) :-
	(retract(counter(N)) -> N2 is N+1 ; N2 = 1),
	assert(node(Id, N2, E, H)),
	assert(counter(N2)).

% Evolve (E, H) as much as possible until (E2, H2)
trans_block(E, H, E2, H2) :- trans_star(E, H, E2, H2), \+ trans(E2, H2, _, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%