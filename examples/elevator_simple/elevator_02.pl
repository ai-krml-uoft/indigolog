%  This is the elevator that appears in the IJCAI-97 paper on ConGolog
%  It uses exogenous actions for temperature, smoke, and call buttons
%  run: ?- indigolog(control).
%
%  Respond to "Exogenous action:" with either "nil." or with one of the
%    exogenous actions below, such as "on(5)." or "heat."
%  The elevator stops when it is parked and all lights are off.

:- [elevator_01].

:- abolish(exog_occurs/1).

% Interface to the outside world via read and write
exog_occurs(A) :- ask_exog_occurs(A).


% Actions for the fan and alarm
prim_action(toggle).             % toggle the fan
prim_action(ring).               % ring the smoke alarm

% Fluents
prim_fluent(temp).               % the temperature in the elevator (number)
prim_fluent(fan).                % the fan (on or off)
prim_fluent(alarm).              % the smoke alarm (on or off)

% Exogenous events
exog_action(heat).               % increase temperature by 1
exog_action(cold).               % decrease temperature by 1
exog_action(smoke).              % smoke enters elevator
exog_action(reset).              % smoke detector alarm is reset
exog_action(on(N)) :- fl(N).     % turn on call button on floor n


% Causal laws
causes_val(heat, temp, X, X is temp+1).
causes_val(cold, temp, X, X is temp-1).

causes_val(toggle, fan, on,  fan=off).
causes_val(toggle, fan, off, fan=on).

causes_val(on(N),  light(N), on,  true).

causes_val(smoke, alarm, on,  true).
causes_val(reset, alarm, off, true).

% Preconditions of prim actions
poss(toggle, true).
poss(ring,   true).

% Initial state
:- abolish(initially/2).
initially(floor, 3).
initially(temp, 2).
initially(fan, off).
initially(light(_), off).   % all lights off initially
initially(alarm, off).

% Definitions of complex conditions
proc(too_hot, temp > 2).
proc(too_cold, -2 > temp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(control, prioritized_interrupts(
        [interrupt(and(too_hot, fan = off), toggle),
         interrupt(and(too_cold, fan = on), toggle),
         interrupt(alarm = on, ring),
         interrupt(n, pending_floor(n), serve_floor(n)),
         interrupt(above_floor(1), down)])).

