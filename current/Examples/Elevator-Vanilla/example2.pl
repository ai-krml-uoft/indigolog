/*  This is the original elevator with no exogenous events, no sensing  */
/*  The smart controller uses search to minimize the up-down motion     */
/*  The dumb controller tries without search but commits too soon       */
/*									*/
/* run: ?- indigolog(dumb_control).					*/
/* run: ?- indigolog(smart_control).					*/

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

  /*  Definitions of complex conditions      */
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).

  /*  Definitions of complex actions      */
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_floor(N), [go_floor(N), open, close, off(N)]).

proc(handle_reqs(Max),      /* handle all elevator reqs in Max steps */
    ndet(  [?(and(neg(some(n,light(n)=on)),Max>=floor-1)), go_floor(1), open],
            pi(n, pi(m, [ ?(and(light(n)=on, m is Max-abs(floor-n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).

proc(minimize_motion(Max),  /* iterative deepening search */
    ndet( handle_reqs(Max), pi(m, [?(m is Max+1), minimize_motion(m)]))).

proc(dumb_control, minimize_motion(0) ).           /* always fails */
proc(smart_control, search(minimize_motion(0)) ).  /* eventually succeeds */
