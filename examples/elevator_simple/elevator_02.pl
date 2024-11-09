/**
  This is the elevator that appears in the IJCAI-97 paper on ConGolog.

  It uses exogenous actions for temperature, smoke, and call buttons
  run:

  Respond to "Exogenous action:" with either "nil." or with one of the
    exogenous actions below, such as "on(5)." or "heat."

  The elevator stops when it is parked and all lights are off.
**/

% Interface to the outside world via read and write
execute(A, SR) :- ask_execute(A, SR).
exog_occurs(A) :- ask_exog_occurs(A).

% number of floors, change for larger buildings
max_floor(3).

fl(N) :- max_floor(M), between(1, M, N).    % the 6 elevator floors

  /*  FLUENTS and CAUSAL LAWS */
prim_fluent(floor).             % the floor the elevator is on
prim_fluent(light(N)) :- fl(N). % call button of floor
prim_fluent(lights).            % call buttons of ALL floors (as a list)

prim_fluent(temp).               % the temperature in the elevator
prim_fluent(fan).                % the fan (on or off)
prim_fluent(alarm).              % the smoke alarm (on or off)

  /*  ACTIONS and PRECONDITIONS*/
prim_action(down).              % elevator down one floor
prim_action(up).                % elevator up one floor
prim_action(open).              % open elevator door
prim_action(close).             % close elevator door
prim_action(off(N)) :- fl(N).   % turn off call button on floor n
prim_action(look).              % check all call buttons
prim_action(look(N)) :- fl(N).  % check button N

% Actions for the fan and alarm
prim_action(toggle).             % toggle the fan
prim_action(ring).               % ring the smoke alarm

% Exogenous events
exog_action(heat).               % increase temperature by 1
exog_action(cold).               % decrease temperature by 1
exog_action(smoke).              % smoke enters elevator
exog_action(reset).              % smoke detector alarm is reset
exog_action(on(N)) :- fl(N).     % turn on call button on floor n

% Sensing axioms for primitive fluents.
senses(look(N), light(N)).      % look(n) asks for the value of light(n)
senses(look, lights).           % ask for current value of all lights

% Preconditions of prim actions
poss(down,    neg(floor = 1)).
poss(up,      neg(floor = M)) :- max_floor(M).
poss(off(N),  and(floor = N, pending_floor(N))).
poss(open, true).
poss(close, true).
poss(toggle, true).
poss(ring,   true).
poss(look(_), true).
poss(look, true).


/* Causal laws / SSA */
causes_val(up,   floor, N, N is floor + 1).
causes_val(down, floor, N, N is floor - 1).
causes_val(off(N), light(N), off, true).
causes_val(on(N),  light(N), on,  true).

causes_val(off(N), lights, L, replace(lights, L, N, 0)).
causes_val(on(N), lights, L, replace(lights, L, N, 1)).

causes_val(heat, temp, X, X is temp+1).
causes_val(cold, temp, X, X is temp-1).

causes_val(toggle, fan, on,  fan=off).
causes_val(toggle, fan, off, fan=on).

causes_val(smoke, alarm, on,  true).
causes_val(reset, alarm, off, true).


  /* Initial state  */
initially(floor, 3).
initially(temp, 2).
initially(fan, off).
initially(light(_), off).   % all lights off initially
initially(alarm, off).

/* ABBREVIATIONS */
proc(too_hot, temp > 2).
proc(too_cold, -2 > temp).
proc(below_floor(N), floor < N).
proc(above_floor(N), floor > N).
proc(pending_floor(N), or(light(N) = on, nth1(N, lights, 1))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% got to floor N
proc(go_floor(N), while(neg(floor = N), if(below_floor(N), up, down))).

% serve floor N
proc(serve_floor(N), [go_floor(N), open, close, off(N)]).

% pick a floor that is pending_floor to be served and serve it
proc(serve_some_floor, pi(n, [?(pending_floor(n)), serve_floor(n)])).

% L = [look(1), look(2), look(3), look(4), look(5), look(6), ...]
proc(check_buttons, L) :- findall(look(N), fl(N), L).

% BASIC CONTROLERS: sense buttons and serve reactively
proc(control(basic(1)),
  [ check_buttons,
    while(or(some(n, pending_floor(n)), above_floor(1)),
      if(some(n, light(n) = on), serve_some_floor, [down, check_buttons])) ]).

proc(control(basic(2)),
  [ look,
    while(or(member(1, lights), above_floor(1)),
      if(member(1, lights), serve_some_floor, [down, look]))]).

% COMPLEX CONTROLER: operate elevator concurrently
proc(control(concurrent), prioritized_interrupts(
        [interrupt(and(too_hot, fan = off), toggle),
         interrupt(and(too_cold, fan = on), toggle),
         interrupt(alarm = on, ring),
         interrupt(n, pending_floor(n), serve_floor(n)),
         interrupt(above_floor(1), down)])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List utility used above
% replace(L, L2, N, V) replaces the Nth element of L with V, giving L2
replace(L1, L2, N, V) :-
        N2 is N-1,
        length(L11, N2),
        append(L11, [_|L12], L1),
        append(L11, [V|L12], L2).
