/* This is the original Golog elevator with no exogenous events, no sensing 	*/
/*										*/
/* run: ?- indigolog(control).							*/

%:-ensure_loaded(indigolog). 	/* Ensure that IndiGolog interpreter is loaded */
%:-load_files(indigolog). 	/* Ensure that IndiGolog interpreter is loaded */

  /* Interface to the outside world via read and write */
execute(A,Sr) :- ask_execute(A,Sr).
exog_occurs(_) :- fail.

  /*  Actions */
prim_action(down).              /* elevator down one floor */
prim_action(up).                /* elevator up one floor */
prim_action(off(N)) :- fl(N).   /* turn off call button on floor n */
prim_action(open).              /* open elevator door */
prim_action(close).             /* close elevator door */

  /*  Fluents */
prim_fluent(floor).             /* the floor the elevator is on */
prim_fluent(light(N)) :- fl(N). /* call button of floor n is on or off */

prim_fluent(on(N,M)) :- fl(N), fl(M). /* call button of floor n is on or off */


fl(N) :- N=1; N=2; N=3; N=4; N=5; N=6.    /* the elevator floors */

  /*  Causal laws */
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).
causes_val(off(N), light(N), off, true).  /* Note: nothing puts light on */

  /*  Preconditions  of prim actions */
poss(down,    neg(floor=1)).
poss(up,      neg(floor=6)).
poss(off(N),  and(floor=N,light(N)=on)).
poss(open, true).
poss(close, true).

  /* Initial state: elevator is at floor 3, lights 2 and 5 are on */
initially(floor,3).
initially(light(1), off).
initially(light(2), on).
initially(light(3), off).
initially(light(4), off).
initially(light(5), on).
initially(light(6), off).

initially(on(_,Y), on):- fl(Y), Y\=2.
initially(on(_,2), off).


  /*  Definitions of complex conditions      */
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), light(N)=on).

  /*  Definitions of complex actions      */
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_a_floor, pi(n, 
    [?(next_floor_to_serve(n)), go_floor(n), open, close, off(n)])).

proc(control, 
   [ while( some(n,light(n)=on), serve_a_floor ),
     go_floor(1),
     open ] ).


