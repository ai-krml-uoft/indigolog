# Vanilla Elevator Controllers

This folder contains simple elevator controller for INDIGOLOG that appeared in Reiter's 2001 Knowledge in Action book, the ConGolog paper in IJCAI-97, and other examples designed by Hector Levesque to showcase INDIGOLOG search operator.

In all cases, an elevator is operating in a building with six (6) floors.  The elevator can go up and down and responds to call buttons on floors being on. Once all call buttons are off, the elevator goes to the first floor and stops.

There are two versions incrementally more complex:

- **Example 1:** the most basic case with no sensing or exogenous events.
- **Example 2:** includes exogenous actions and sensing.

Files `main_xx.pl` are provided to start the applications; remember they need to be consulted in the context of a configuration file (defining some global variables and predicates).

## Example 1: Basic elevator - no sensing or exogenous events

There are three controllers:

- `basic`: reactive, not optimised, just pick a pending floor and serve it.
- `dumb`: tries to minimize number of moves, but fails as it does not lookahead.
- `smart`: minimizes number of moves by using search lookahead.

You can select them interactively by querying `main/0` or directly via `main/1` (with the controller name id).

The _basic_ controller just picks pending floors randomly one by one until all are served, then park at floor 1.

```shell
❯ swipl config.pl examples/elevator_simple/main_01.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- main(basic).
down
open
close
off(2)
up
up
up
open
close
off(5)
up
up
up
up
open
close
off(9)
down
down
down
down
down
down
down
down
open

26 actions.
true
```

The _dumb_ and _smart_ controllers are meant to _minimize the number of moves of the elevator_. However, the dumb controller does not perform look-ahead search and hence fails to find a complete execution, whereas the _smart_ controller uses look-ahead search until a full execution that solves the problem is found:

```shell
$ swipl config.pl examples/elevator_simple/main_01.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- main(dumb).
false.

?- main.
Controllers available: [basic,dumb,smart]
Select controller: smart.

Executing controller: *smart*
up
up
open
close
off(5)
up
up
up
up
open
close
off(9)
down
down
down
down
down
down
down
open
close
off(2)
down
open

24 actions.
true.
```

## Example 2: Control under exogenous events

There are three type of controllers. By default it is a very small building with 3 floors:

```prolog
% number of floors, change for larger buildings
max_floor(3).
```

The `basic(1)` and `basic(2)` controllers are reactive non-optimized controllers that once all pending floors are known (via sensing), it keeps serving them in any order. Sensing of lights is done at the start, and once all pending floors are served and elevator travels down to ground floor. While `basic(1)` uses a sequence of `look(N)` sensing action (sensing outcome `on` or `off`), one per floor, controller `basic(2)` senses status of all lights as a list of 0s and 1s.

```prolog
$ swipl config.pl examples/elevator_simple/main_02.pl
?- main(basic(1)).
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look(1) - Sensing outcome term (ending with '.'):
|: off.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look(2) - Sensing outcome term (ending with '.'):
|: on.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look(3) - Sensing outcome term (ending with '.'):
|: off.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(2)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look(1) - Sensing outcome term (ending with '.'):
|: on.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look(2) - Sensing outcome term (ending with '.'):
|: off.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look(3) - Sensing outcome term (ending with '.'):
|: off.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(1)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.

20 actions.
true .
```

Here is an execution of `basic(2)` with sensing outcomes as lists of `0` and `1`:

```prolog
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(2)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look - Sensing outcome term (ending with '.'):
|: [1,0,1].
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(1)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
up
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
up
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(3)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look - Sensing outcome term (ending with '.'):
|: [off,off,off].
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
look - Sensing outcome term (ending with '.'):
|: [off,off,off].
Exogenous input term (ending with '.'; true for none; help for list):
|: true.

23 actions.
true .
```

### Concurrent controller

The complex controller `concurrent` is the one that appeared in the ConGolog paper and uses concurrency to serve floors and react to temperature and smoke eventualities. There is no sensing, but exogenous action `on(N)` may set a floor in pending mode.

Here is a complex run:

```prolog
?- main.
Controllers available: [basic(1),basic(2),concurrent]
Select controller: 
|: concurrent.

Executing controller: *concurrent*
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
start_interrupts
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: on(3).
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
up
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: on(2).
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(3)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
open
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
close
Exogenous input term (ending with '.'; true for none; help for list):
|: heat.
Exogenous input term (ending with '.'; true for none; help for list):
|: heat.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
toggle
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
off(2)
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
down
Exogenous input term (ending with '.'; true for none; help for list):
|: smoke.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
ring
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
ring
Exogenous input term (ending with '.'; true for none; help for list):
|: reset.
Exogenous input term (ending with '.'; true for none; help for list):
|: true.
stop_interrupts
Exogenous input term (ending with '.'; true for none; help for list):
|: true.

21 actions.
true .
```