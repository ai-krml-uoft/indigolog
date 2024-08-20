%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Standalone INDIGOLOG implemenation

    Part of the INDIGOLOG system

    Refer to root directory for license, documentation, and inforamtion.

This file contains a full **vanilla** implementation of INDIGOLOG,
including online executor, language constructs, and temporal projector

In addition to a ConGolog program, users provide these predicates:

      prim_fluent(fluent),             for each primitive fluent
      prim_action(action),             for each primitive action
      exog_action(action),             for each exogenous action
      senses(action, fluent),           for each sensing action
      poss(action, cond)                when cond, action is executable
      initially(fluent, value)          fluent has value in S0
      causes_val(action, fluent, value, cond)
            when cond holds, doing act causes fluent to have value

      execute(action, sensing_result)   do the action, return the result
            can use ask_execute
      exog_occurs(action)              return an exog action
            can use ask_exog_occurs (or fail, if none)
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- style_check(-discontiguous).	% SWI specific!

:-dynamic senses/2.
:-dynamic exog_action/1.

:-multifile prim_action/1, causes_val/4, poss/2, proc/2, prim_fluent/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
                     MAIN LOOP: indigolog and indixeq

Top-level execution cycle: sense, plan, execute
Sensing outcomes are inserted as actions of  the form e(fluent, value)
Execution and sensing is done via console I/O
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
indigolog(E) :- indigo(E, []).

% (1)- In each single step ask for an exogenous action, check it and
%	continue execution inserting that exogenous action
indigo(E, H) :- exog_occurs(Act), exog_action(Act), !, indigo(E, [Act|H]).

% (2) - Find a signle step (trans), execute it, commit and continue
indigo(E, H) :- trans(E, H, E1, H1), indixeq(H, H1, H2), !, indigo(E1, H2).

% (3) - If E is final write the length of history H
indigo(E, H) :- final(E, H), nl, length(H, N), write(N), write(" actions."), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% indixeq(H1, H2, H3): Implementation of execution of an action.
% 	H1 is the original history, H2 is H1 with the new action to be
%	executed and H3 is the resulting history after executing such new
% 	action.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) - No action was performed so we dont execute anything
indixeq(H, H, H).
% (2) - The action is not a sensing one: execute and ignore its sensing
indixeq(H, [A|H], [A|H]) :- \+ senses(A, _), execute(A, _).
% (3) - The action is a sensing one for fluent F: execute sensing action
indixeq(H, [A|H], [e(F, SR), A|H]) :- senses(A, F), execute(A, SR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exog_occurs(Act) and execute(Act, Sr):
% 	predicates that make contact with the outside world.
%	Here are two basic versions using read and write that the domain
%	may use as a simulated environment.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ask_exog_occurs(A) :-
       write("Exogenous input (ending with "."): "), read(A).

ask_execute(A, _) :-  \+ senses(A, _), !, write(A), nl.
ask_execute(A, SR) :-  senses(A, _),
       format("~w - Sensing outcome: ", [A]), read(SR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
                     TRANS and FINAL: language semantics

       Operational semantics of the IndiGolog language.
       Basically ConGolog semantics + search operator
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       /* (a) - CONGOLOG */
final(conc(E1, E2), H) :- final(E1, H), final(E2, H).
final(pconc(E1, E2), H) :- final(E1, H), final(E2, H).
final(iconc(_), _).

trans(conc(E1, E2), H, conc(E, E2), H1) :- trans(E1, H, E, H1).
trans(conc(E1, E2), H, conc(E1, E), H1) :- trans(E2, H, E, H1).
trans(pconc(E1, E2), H, E, H1) :-
    trans(E1, H, E3, H1) -> E = pconc(E3, E2) ; (trans(E2, H, E3, H1), E = pconc(E1, E3)).
trans(iconc(E), H, conc(E1, iconc(E)), H1) :- trans(E, H, E1, H1).

       /* (b) - GOLOG */
final([], _).
final([E|L], H) :- final(E, H), final(L, H).
final(ndet(E1, E2), H) :- final(E1, H) ; final(E2, H).
final(if(P, E1, E2), H) :- holds(P, H) -> final(E1, H) ; final(E2, H).
final(star(_), _).
final(while(P, E), H) :- \+ holds(P, H) ; final(E, H).
final(pi(V, E), H) :- subv(V, _, E, E2), final(E2, H).
final(E, H) :- proc(E, E2), final(E2, H).

trans([E|L], H, [E1|L], H2) :- trans(E, H, E1, H2).
trans([E|L], H, E1, H2) :- \+ L = [], final(E, H), trans(L, H, E1, H2).
trans(?(P), H, [], H) :- holds(P, H).
trans(ndet(E1, E2), H, E, H1) :- trans(E1, H, E, H1) ; trans(E2, H, E, H1).
trans(if(P, E1, E2), H, E, H1) :- holds(P, H) -> trans(E1, H, E, H1) ; trans(E2, H, E, H1).
trans(star(E), H, [E1, star(E)], H1) :- trans(E, H, E1, H1).
trans(while(P, E), H, [E1, while(P, E)], H1) :- holds(P, H), trans(E, H, E1, H1).
trans(pi(V, E), H, E1, H1) :- subv(V, _, E, E2), trans(E2, H, E1, H1).
trans(E, H, E1, H1) :- proc(E, E2), trans(E2, H, E1, H1).
trans(E, H, [], [E|H]) :- prim_action(E), poss(E, P), holds(P, H).

       /* (c) -  SEARCH (ignoring exogenous or other concurrent actions) */
/* If (E, H) is a final state then finish. Otherwise, look for a straight
   path (E1, L) without looking at exogenous actions */
final(search(E), H) :- final(E, H).
trans(search(E), H, followpath(E1, L), H1) :- trans(E, H, E1, H1), findpath(E1, H1, L).

/* Look for a good path without looking at exogenous actions */
findpath(E, H, [E, H]) :- final(E, H).
findpath(E, H, [E, H|L]) :- trans(E, H, E1, H1), findpath(E1, H1, L).


/* When we have a followpath(E, L), try to advance using the list L
   in an offline manner.
   If it is not possible to advance any more redo the search to continue */
final(followpath(E, [E, H]), H) :- !.
final(followpath(E, _), H) :- final(E, H).  /* off path; check again */
trans(followpath(E, [E, H, E1, H1|L]), H, followpath(E1, [E1, H1|L]), H1) :- !.
trans(followpath(E, _), H, E1, H1) :- trans(search(E), H, E1, H1).  /* redo search */

       /* (d) -  INTERRUPTS */
prim_action(start_interrupts).
prim_action(stop_interrupts).
prim_fluent(interrupts).
causes_val(start_interrupts, interrupts, running, true).
causes_val(stop_interrupts, interrupts, stopped, true).
poss(start_interrupts, true).
poss(stop_interrupts,  true).

proc(interrupt(V, Trigger, Body),            /* version with variable */
    while(interrupts = running, pi(V, if(Trigger, Body, ?(neg(true)))))).
proc(interrupt(Trigger, Body),              /* version without variable */
    while(interrupts = running, if(Trigger, Body, ?(neg(true))))).
proc(prioritized_interrupts(L), [start_interrupts, E]) :- expand_interrupts(L, E).
expand_interrupts([], stop_interrupts).
expand_interrupts([X|L], pconc(X, E)) :- expand_interrupts(L, E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
       holds/2: temporal projector

       holds(+P, ++H): formula P is true at history H

       P can be open, that is, can have variables
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
holds(and(P1, P2), H) :- !, holds(P1, H), holds(P2, H).
holds(and([P]), H) :- !, holds(P, H).
holds(and([P|L]), H) :- !, holds(P, H), holds(and(L), H).
holds(or(P1, P2), H) :- !, (holds(P1, H) ; holds(P2, H)).
holds(or(L), H) :- is_list(L), !, member(P, L), holds(P, H).
holds(neg(P), H) :- !, \+ holds(P, H).   /* Negation by failure */
holds(some(V, P), H) :- !, subv(V, _, P, P1), holds(P1, H).
holds(P, H) :- proc(P, P1), holds(P1, H).
holds(P, H) :- \+ proc(P, _), subf(P, P1, H), call(P1).

%!      subv(++X2, ++X1, +T1, -T2) is det
%       T2 is T1 with X1 replaced by X2 - T2 = T1|_{X1/X2}
%  NOW parts of utils.pl lib module -- uncomment if loaded stand-alone
% subv(X1, X2, T1, T2) :- var(T1), T1 == X1, !, T2 = X2.
% subv(_, _, T1, T2) :- var(T1), !, T2 = T1.
% subv(X1, X2, T1, T2) :- T1 == X1, !, T2 = X2.
% subv(X1, X2, T1, T2) :- T1 =..[F|L1], maplist(subv(X1, X2), L1, L2), T2 =..[F|L2].

       /*  P2 is P1 with all fluents replaced by their values  */
subf(P1, P2, _) :- (var(P1) ; integer(P1)), !, P2 = P1.
subf(P1, P2, H) :- prim_fluent(P1), has_val(P1, P2, H).
subf(P1, P2, H) :- \+ prim_fluent(P1), P1=..[F|L1],
       maplist({H}/[A, B]>>subf(A, B, H), L1, L2), P2=..[F|L2].


% has_val(F, V, H):  Fluent F has value V in history H.
has_val(F, V, []) :- initially(F, V).
has_val(F, V, [Act|H]) :- sets_val(Act, F, V1, H) -> V = V1 ; has_val(F, V, H).


%!     sets_val(+Act, +F, -V, ++H) is nondet
%
%      Action Act causes fluent F to be set to V in history H.
%	Act can be either an exogenous action e(F, V) or a standard
%	action with a successor state axiom causes_val(Act, F, V, P).
sets_val(Act, F, V, H) :- Act = e(F, V) ; (causes_val(Act, F, V, P), holds(P, H)).
