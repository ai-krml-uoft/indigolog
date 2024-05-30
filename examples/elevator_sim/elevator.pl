
/*
  Complex elevator domain as per the original INDIGOLOG code

  @author Sebastian Sardina - ssardina@gmail.com  (2001-2006)
		(based on code previously written by Hector Levesque)

  This file contains 4 of the controllers from the original code
  written by Hector Levesque for the 1st INDIGOLOG version:

  1. controller(1) : (example2.pl in the original INDIGOLOG)
  The dumb controller tries without search but commits too soon

  2. controller(2) : (example2.pl in the original INDIGOLOG)
  The smart controller uses search to minimize the up-down motion

  3. controller(3) : (example3.pl in the original INDIGOLOG)
  This is the elevator that appears in the IJCAI-97 paper on ConGolog
  It uses exogenous actions for temperature, smoke, and call buttons

  4. controller(4) : (example4.pl in the original INDIGOLOG)
  This is the elevator with no exogenous events, but with sensing
  actions for each call button of the elevator
*/
:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.

% There is nothing to do caching on (required becase cache/1 is static)
cache(_) :- fail.

  /* DOMAINS-SORTS AVAILABLE */
max_floor(10).
fl(N) :- max_floor(M), between(1, M, N).

  /*  FLUENTS and CAUSAL LAWS */
fun_fluent(floor).              % the floor the elevator is on
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).

rel_fluent(door_open).  % true if door is open
causes_true(open,   door_open, true).
causes_false(close,  door_open, true).

fun_fluent(temp).               % the temperature of the elevator
causes_val(heat, temp, X, X is temp + 5).
causes_val(cold, temp, X, X is temp - 5).

rel_fluent(fan).                   % true if fan is on
causes_true(toggle,   fan, neg(fan)).
causes_false(toggle,  fan, fan).

rel_fluent(alarm).              % true if alarm is on
causes_true(smoke, alarm, true).
causes_false(reset, alarm, true).

rel_fluent(light(N)) :- fl(N).  % floor is pending
causes_true(on(N),  light(N), true).
causes_false(off(N), light(N), true).
senses(look(N), light(N)).     % checks if light(N) is true


rel_fluent(new_request).          % true if some light becomes true
causes_true(on(N),   new_request, neg(light(N))).


  /*  ACTIONS and PRECONDITIONS*/
prim_action(down).
poss(down, and(neg(door_open), neg(floor = 1))).

prim_action(up).
poss(up, and(neg(door_open), neg(floor = N))) :- max_floor(N).

prim_action(toggle).  % toggle ring alarm
poss(toggle, true).

prim_action(ring).    % do one ring
poss(ring, true).

prim_action(off(N)) :- fl(N).    % turn off call button on floor n
poss(off(N), and(floor = N, light(N))).
prim_action(open).		% open door
poss(open, true).

prim_action(close).   % close door
poss(close, true).

prim_action(look(N)) :- fl(N).  % sense floor N light
poss(look(_), true).

prim_action(say(_)).
poss(say(_), true).


  /* EXOGENOUS ACTIONS */
exog_action(heat).            % increase temperature
exog_action(cold).            % decrease temperature
exog_action(smoke).           % smoke enters elevator
exog_action(reset).           % smoke detector alarm is reset
exog_action(on(N)) :- fl(N).  % turn on call button on floor n

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).

  /* ABBREVIATIONS */
proc(too_hot, temp > 22).
proc(too_cold, temp < 16).
proc(above_floor(N), floor > N).
proc(below_floor(N), floor < N).
proc(pending_floor(N), light(N)).
proc(some_pending, some(n, light(n))).


  /* INITIAL STATE */
initially(floor, 2).
initially(light(N), true) :- fl(N), member(N, [1, 3, 7, 8]).
initially(light(N), false) :- fl(N), \+ initially(light(N), true).
initially(temp, 2).
initially(fan, false).
initially(alarm, false).
initially(new_request, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(go_floor(N),
  [if(door_open, close, []),
    while(neg(floor = N), if(below_floor(N), up, down))]).
proc(serve_floor(N), [go_floor(N), open, close, off(N)]).

% pick a floor that is pending to be served and serve it
proc(serve_some_floor, pi(n, [?(pending_floor(n)), serve_floor(n)])).


% DUMB: just kep serving some pending floor and then go down
%     NO REACTION TO EXOGENOUS ACTIONS OR SENSING
proc(control(dumb),
   [ while(some_pending, serve_some_floor ),
     go_floor(1), % go to floor 1 to park
     open ] ).

% SMART: build a shorter plan and then execute it all
%     NO REACTION TO EXOGENOUS ACTIONS OR SENSING
proc(control(smart), search(minimize_motion(0)) ).  /* eventually succeeds */

proc(minimize_motion(Max),  /* iterative deepening search */
    ndet( handle_reqs(Max), pi(m, [?(m is Max + 1), minimize_motion(m)]))).

proc(handle_reqs(Max),      /* handle all elevator reqs in Max steps */
    ndet(  [?(and(neg(some_pending), Max >= floor - 1)), go_floor(1), open],
            pi(n, pi(m, [ ?(and(light(n), m is Max - abs(floor-n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).

/* REACTIVE CONTROLLER:

This is an extension of the elevator that appears in the IJCAI-97 AND
AIJ-03 papers on ConGolog

It uses exogenous actions for temperature, smoke, and call buttons. It
also uses prioritized interrupts to handle the exogenous events and the
call buttons.

The serving of floors is still naive: just serve some pending floor

It is extended to:

  - track the state of the door
  - wait at ground floor for more requests

*/
proc(control(congolog), [prioritized_interrupts(
        [interrupt(and(too_hot, neg(fan)), toggle),
         interrupt(and(too_cold, fan), toggle),
         interrupt(alarm, ring),
         interrupt(n, pending_floor(n), serve_floor(n)),
         interrupt(above_floor(1), down),
         interrupt(neg(door_open), open),
        %  interrupt(true, say("Waiting at gound floor"))])]).
         interrupt(true, ?(wait_exog_action))])]).


%  REACTIVE + PLANNING CONTROLLERS
proc(control(indigolog), [prioritized_interrupts(
        [interrupt(and(too_hot, neg(fan)), toggle),
         interrupt(and(too_cold, fan), toggle),
         interrupt(alarm, ring),
        %  interrupt(some_pending, search(minimize_motion(0))),
          interrupt(some_pending,
            [ unset(new_request),
              gexec(neg(new_request), search(minimize_motion(0), "Searching for plan"))
              ]),
         interrupt(above_floor(1), down),
         interrupt(neg(door_open), open),
        %  interrupt(true, say("Waiting at gound floor"))])]).
         interrupt(true, ?(wait_exog_action))])]).


proc(control(indigolog_ends), [prioritized_interrupts(
        [interrupt(and(too_hot, neg(fan)), toggle),
         interrupt(and(too_cold, fan), toggle),
         interrupt(alarm, ring),
        %  interrupt(some_pending, search(minimize_motion(0))),
          interrupt(some_pending,
            [ unset(new_request),
              gexec(neg(new_request), search(minimize_motion(0), "Searching for plan"))
              ]),
         interrupt(above_floor(1), down),
         interrupt(neg(door_open), open)]),
         say("Waiting at gound floor, thanks...")]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
