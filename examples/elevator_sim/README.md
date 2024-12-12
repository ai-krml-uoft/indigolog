# Elevator Controller Simulator

This are variants of the Elevator Controller example showcased in the CONGOLOG paper:

* Giuseppe De Giacomo, Yves Lespérance, Hector J. Levesque: [ConGolog, a concurrent programming language based on the situation calculus](https://www.sciencedirect.com/science/article/pii/S000437020000031X?via%3Dihub). Artificial Intelligence 121(1-2): 109-169 (2000)

Beyond the basic GOLOG constructs, the example depicts the usefulness of using concurrency, prioritized processes, interrupts, and exogenous actions to tackle real-world scenarios. Besides serving the requested floors, the elevator needs to appropriately handle changes in temperature (via a fan) and emegency situations by ringing the alarm.

In addition, some controllers make use of INDIGOLOG search operator to optimise the behavior of the elevator.

The example executes in a simulated environment via simulator device manager `dev/env_sim.pl`. The simulator includes a simple Python-based TCLK/TK where the user can issue exogenous actions:

```prolog
- `heat`: temperature went up.
- `cold`: temperature went down.
- `smoke`: soke has been sensed.
- `reset`: smoke situation has been reset.
- `on(N)`: floor `N` has requested service.
```

The `end_indi` exogenous actions will finish the execution smoothly.

## Running the elevator system

First, we should consult the application as follows:

```shell
$ swipl config.pl examples/elevator_sim/main.pl
```

This will load the domain specification, but also the whole INDIGOLOG architecture (interpreter, temporal projector, devices, libraries, etc.)

We can call `main/0` to get a list of controllers available and select (remember the input has to finish with full stop `.`):

```shell
?- main.
Controllers available: [dumb,smart,congolog,congolog_smart,congolog_smart_ends]
1. dumb
2. smart
3. congolog
4. indigolog
5. indigolog_ends


Select controller: 5.

Executing controller: *indigolog_ends*
INIT: Starting ENVIRONMENT MANAGER...
EM(1): Openinig EM server socket...
INFO(5,APP): Command to initialize device simulator: xterm -e [-e,swipl,-t,start,/home/ssardina/PROJECTS/sitcalc/IndiGolog/indigolog.git/devices/dev_sim.pl,--host,localhost,--port,8000]
EM(1): Device simulator initialized at ip(127,0,0,1)
INIT: ENVIRONMENT MANAGER was started successfully.
INIT: Starting PROJECTOR EVALUATOR...
INIT: PROJECTOR was started successfully.
INFO(5): History updated to: []
INIT: Starting to execute main program
INFO(2): Sending action for execution: unset(new_request)
ACTION: Action EXECUTED: [[unset(new_request),1],sensing(ok)]
INFO(5): History updated to: [unset(new_request)]
INFO(2): Waiting step for 1 seconds...
PROGRAM(3): Start search on: Searching for plan
PROGRAM(3): Search finished! PLAN FOUND: Searching for plan
INFO(2): Sending action for execution: up
ACTION: Action EXECUTED: [[up,2],sensing(ok)]
INFO(5): History updated to: [up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: open
ACTION: Action EXECUTED: [[open,3],sensing(ok)]
INFO(5): History updated to: [open,up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: close
ACTION: Action EXECUTED: [[close,4],sensing(ok)]
INFO(5): History updated to: [close,open,up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: off(3)
...
...
INFO(2): Waiting step for 1 seconds...
INDI(0): Received sys actions: [end_indi]
INFO(0): Request for smooth END
PROGRAM: Program has executed to completion!! History done:
	 [off(1),close,open,down,down,down,down,down,down,down,off(8),close,open,up,off(7),close,open,up,up,up,up,off(3),close,open,up,unset(new_request)]
END: Execution finished. Closing modules...
END: INDIGOLOG is finishing...
END: Finalizing PROJECTOR...
END: PROJECTOR was finalized successfully.
END: Finalizing ENVIRONMENT MANAGER...
EM(1): EM completed with 26 executed actions
END: ENVIRONMENT MANAGER was finalized successfully.
END: Everything finished - HALTING TOP-LEVEL CONTROLLER
```

A controller can also be started via INDIGOLOG top-level predicate `indigolog/1`.

### INDIGOLOG Smart Controller

Controllers `indigolog` is probably the most complete and interesting:

```prolog
proc(control(indigolog), [prioritized_interrupts(
        [interrupt(and(too_hot, neg(fan)), toggle),
         interrupt(and(too_cold, fan), toggle),
         interrupt(alarm, ring),
         interrupt(some_pending,	% smart planning on serving floors
            [ unset(new_request),
              gexec(neg(new_request), search(minimize_motion(0), "Searching for plan"))
              ]),
         interrupt(above_floor(1), down),
         interrupt(neg(door_open), open),
		 	% nothing to serve and in floor 1: wait for requests
         interrupt(true, [say("Waiting at gound floor"), ?(wait_exog_action)])])]).
```


At its core, it is basically the controller showcased with CONGOLOG (extended with a door and tighter preconditions), but with two enhancements:

1. When serving pending floors, the controller devise an efficient plan minimizing the number of moves. This is done via INDIGOLOG local loo-ahead search operator. In addition, the execution of such plan is guarded (via extension cosntruct `gexec/2`) to continue as long as no new floor request has been issued. This allows for re-planning when a new floor is requested. **OBS:** This assumes requests cannot be canceled.
2. When no floors are pending to be served, the elevator parks with the door open at floor 1, and waits for exogenous actions to ocurr, via special test `?(wait_exog_action)`.

Here is a run, calling `indigolog/1` directly:

```shell
$ swipl config.pl examples/elevator_sim/main.pl
SYSTEM(0): Debug level set to 5
SYSTEM(0): Debug level for module em set to 1
INFO(0): Set wait-at-action enable to: 1 seconds.
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.5)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- indigolog(control(indigolog)).
INIT: Starting ENVIRONMENT MANAGER...
EM(1): Openinig EM server socket...
INFO(5,APP): Command to initialize device simulator: xterm -e [-e,swipl,-t,start,/home/ssardina/PROJECTS/sitcalc/IndiGolog/indigolog.git/devices/dev_sim.pl,--host,localhost,--port,8000]
EM(1): Device simulator initialized at ip(127,0,0,1)
INIT: ENVIRONMENT MANAGER was started successfully.
INIT: Starting PROJECTOR EVALUATOR...
INIT: PROJECTOR was started successfully.
INFO(5): History updated to: []
INIT: Starting to execute main program
INFO(2): Sending action for execution: unset(new_request)
ACTION: Action EXECUTED: [[unset(new_request),1],sensing(ok)]
INFO(5): History updated to: [unset(new_request)]
INFO(2): Waiting step for 1 seconds...
PROGRAM(3): Start search on: Searching for plan
PROGRAM(3): Search finished! PLAN FOUND: Searching for plan
INFO(2): Sending action for execution: up
ACTION: Action EXECUTED: [[up,2],sensing(ok)]
INFO(5): History updated to: [up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: open
ACTION: Action EXECUTED: [[open,3],sensing(ok)]
INFO(5): History updated to: [open,up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: close
ACTION: Action EXECUTED: [[close,4],sensing(ok)]
INFO(5): History updated to: [close,open,up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: off(3)
ACTION: Action EXECUTED: [[off(3),5],sensing(ok)]
INFO(5): History updated to: [off(3),close,open,up,unset(new_request)]
INFO(2): Waiting step for 1 seconds...
INFO(2): Sending action for execution: up
ACTION: Action EXECUTED: [[up,6],sensing(ok)]
INFO(5): History updated to: [up,off(3),close,open,up,unset(new_request)]\
INIT: PROJECTOR was started successfully
INIT: PROJECTOR was started successfully
...
...
...
```
