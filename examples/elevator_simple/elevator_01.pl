/**
  This is the original Golog elevator with no exogenous events and no sensing.

  Serves each floor whose call button is on initially, then park the elevator.

**/

% Interface to the outside world via read and write
execute(A, SR) :- ask_execute(A, SR).
exog_occurs(_) :- fail.

max_floor(10).

fl(N) :- max_floor(M), between(1, M, N).    % the 6 elevator floors

% Actions
prim_action(down).              % elevator down one floor
prim_action(up).                % elevator up one floor
prim_action(open).              % open elevator door
prim_action(close).             % close elevator door
prim_action(off(N)) :- fl(N).   % turn off call button on floor n

% Fluents
prim_fluent(floor).             % the floor the elevator is on (1 to 6)
prim_fluent(light(N)) :- fl(N). % call button of floor n (on or off)

% Causal laws
causes_val(up,   floor, N, N is floor + 1).
causes_val(down, floor, N, N is floor - 1).
causes_val(off(N), light(N), off, true).  % Note: nothing turns a light on

% Preconditions of prim actions
poss(down,    neg(floor = 1)).
poss(up,      neg(floor = M)) :- max_floor(M).
poss(off(N),  and(floor = N, light(N) = on)).
poss(open, true).
poss(close, true).

% Initial state: elevator is at floor 3, and lights 2 and 5 are on
initially(floor, 3).
initially(light(N), on) :- member(N, [2, 5, 9]).
initially(light(N), off) :- fl(N), \+ initially(light(N), on).

% Definitions of complex conditions
proc(below_floor(N), floor < N).
proc(above_floor(N), floor > N).
proc(pending_floor(N), light(N) = on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% got to floor N
proc(go_floor(N), while(neg(floor = N), if(below_floor(N), up, down))).

% serve floor N
proc(serve_floor(N), [go_floor(N), open, close, off(N)]).

% pick a floor that is pending to be served and serve it
proc(serve_some_floor, pi(n, [?(pending_floor(n)), serve_floor(n)])).


% CONTROLER 1: while there are some light on, pick one and serve it
proc(control(basic),
   [ while( some(n, light(n) = on), serve_some_floor ),
     go_floor(1), % go to floor 1 to park
     open ] ).

% CONTROLERS 2 & 3: try to minimize the number of moves done to serve all pending floors
proc(control(dumb), minimize_motion(0) ).           % always fails, no lookahead search!
proc(control(smart), search(minimize_motion(0)) ).  % eventually succeeds

% handle all elevator pending requests in Max steps
proc(handle_reqs(Max),
    ndet(  [?(and(neg(some(n, light(n) = on)), Max >= floor-1)), go_floor(1), open], % no pending, park!
            pi(n, pi(m, [ ?(and(light(n) = on, m is Max - abs(floor - n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).

proc(minimize_motion(Max),  % iterative deepening search starting with Max moves
    ndet( handle_reqs(Max), pi(m, [?(m is Max + 1), minimize_motion(m)]))).


