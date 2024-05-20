/*
  Complex elevator domain as per the original IndiGolog code

  @author Sebastian Sardina - ssardina@gmail.com  (2001-2006)
		(based on code previously written by Hector Levesque)

  This file contains 4 of the controllers from the original code
  written by Hector Levesque for the 1st IndiGolog version:

  1. controller(1) : (example2.pl in the original IndiGolog)
  The dumb controller tries without search but commits too soon

  2. controller(2) : (example2.pl in the original IndiGolog)
  The smart controller uses search to minimize the up-down motion

  3. controller(3) : (example3.pl in the original IndiGolog)
  This is the elevator that appears in the IJCAI-97 paper on ConGolog
  It uses exogenous actions for temperature, smoke, and call buttons

  4. controller(4) : (example4.pl in the original IndiGolog)
  This is the elevator with no exogenous events, but with sensing
  actions for each call button of the elevator
*/
:- dynamic controller/1.
:- discontiguous fun_fluent/1, rel_fluent/1, proc/2.

/* DOMAINS-SORTS AVAILABLE */
fl([1,2,3,4,5,6,7,8,9,10]).       	% possible floors
dir([up,down]).                    	% possible directions
temperature([15,20,25,30,35]).     	% possible temperatures

% There is nothing to do caching on (required becase cache/1 is static)
cache(_):-fail.

floor(N) :- domain(N,fl).      /* N is a legal floor */


  /*  FLUENTS and CAUSAL LAWS */
fun_fluent(floor).              /* the floor the elevator is on */
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).

fun_fluent(temp).               /* the temperature of the elevator */
causes_val(heat, temp, X, X is temp+5).
causes_val(cold, temp, X, X is temp-5).

rel_fluent(fan).                   % the fan is on or off
causes_true(toggle,   fan, neg(fan)).
causes_false(toggle,  fan, fan).

fun_fluent(alarm).              /* the smoke alarm is on or off */
causes_val(smoke, alarm, on,  true).
causes_val(resetAlarm, alarm, off, true).

fun_fluent(light(N)) :- floor(N).  /* call button of floor n is on or off */
causes_val(on(N),  light(N), on,  true).
causes_val(off(N), light(N), off, true).
senses(look(N), light(N)).      /* look(n) asks for the value of light(n) */


  /*  ACTIONS and PRECONDITIONS*/
prim_action(down).               /* elevator down one floor */
poss(down,   neg(floor=1)).

prim_action(up).                 /* elevator up one floor */
poss(up,     neg(floor=10)).

prim_action(toggle).             /* toggle the fan */
poss(toggle, true).

prim_action(ring).               /* ring the smoke alarm */
poss(ring,   true).

prim_action(off(N)) :- floor(N).    /* turn off call button on floor n */
poss(off(N), and(floor=N,light(N)=on)).

prim_action(open).		/* open door */
poss(open, true).

prim_action(close).             /* close door */
poss(close, true).

prim_action(close).             /* close door */
poss(close, true).

prim_action(look(N)) :- floor(N).  /* sensing action: check call button on floor n */
poss(look(_), true).



exog_action(heat).               /* increase temperature by 1 */
exog_action(cold).               /* decrease temperature by 1 */
exog_action(smoke).              /* smoke enters elevator */
exog_action(resetAlarm).              /* smoke detector alarm is reset */
exog_action(on(N)) :- floor(N).     /* turn on call button on floor n */

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).

/* ABBREVIATIONS */
proc(too_hot, temp>2).
proc(too_cold, -2>temp).
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), light(N)=on).


/* INITIAL STATE: elevator is at floor 3, lights 2 and 6 are on */
initially(floor,2).
initially(temp,2).
initially(fan,false).
initially(light(N),off) :- floor(N), N\=1, N\=3.
initially(light(3),on).
initially(light(1),on).
initially(alarm,off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN PROCEDURE FOR INDIGOLOG
proc(main,  controller(N)) :- controller(N), !.
proc(main,  controller(3)). % default one

proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_floor(N), [go_floor(N), off(N)]).

proc(handle_reqs(Max),      /* handle all elevator reqs in Max steps */
    ndet(  [?(and(neg(some(n,light(n)=on)),Max>=floor-1)), go_floor(1), open],
            pi(n, pi(m, [ ?(and(light(n)=on, m is Max - abs(floor-n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).




/*  This is the original elevator with no exogenous events, no sensing  */
/*  The smart controller uses search to minimize the up-down motion     */
/*  The dumb controller tries without search but commits too soon       */
proc(controller(1), dumb_control).
proc(controller(2), smart_control).

proc(minimize_motion(Max),  /* iterative deepening search */
    ndet( handle_reqs(Max), pi(m, [?(m is Max+1), minimize_motion(m)]))).

proc(dumb_control, minimize_motion(0) ).           /* always fails */
proc(smart_control, search(minimize_motion(0)) ).  /* eventually succeeds */


/*  This is the elevator that appears in the IJCAI-97 paper on ConGolog */
/*  It uses exogenous actions for temperature, smoke, and call buttons  */
proc(controller(3), prioritized_interrupts(
        [interrupt(and(too_hot,neg(fan)), toggle),
         interrupt(and(too_cold,fan), toggle),
         interrupt(alarm=on, ring),
         interrupt(n, next_floor_to_serve(n), serve_floor(n)),
         interrupt(above_floor(1), down)])).

/*  This is the elevator with no exogenous events, but with sensing   	*/
/*  actions for each call button of the elevator                      	*/
proc(controller(4),
  [ check_buttons,
    while(or(some(n,light(n)=on), above_floor(1)),
      if(some(n,light(n)=on), serve_a_floor, [down, check_buttons])) ]).
proc(serve_a_floor, pi(n, [?(next_floor_to_serve(n)), go_floor(n), off(n)])).
proc(check_buttons,
	[look(1), look(2), look(3), look(4), look(5), look(6), look(7), look(8), look(9), look(10)]).



proc(controller(5), search(minimize_motion(0),[]) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: ElevatorSim-BAT/elevator.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

trans(searchn(minimize_motion(0),[]),[],E,H),transn(E,H,E2,H2,10),!,ttrans(E2,[on(3)|H2],EF,HF), final(EF,HF).


 trans(searchn(minimize_motion(0),[]),[],E,H),transn(E,H,E2,H2,10),!,trans(E2,[on(3)|H2],E3,H3), transn(E3,H3,E4,H4,5), ttrans(E4,[smoke|H4],EF,HF), final(EF,HF).


*/


