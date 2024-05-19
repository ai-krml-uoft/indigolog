# Vanilla Elevator Controllers

This folder contains five versions of a simple elevator controller for IndiGolog that appeared in Reiter's 2001 Knowledge in Action book, the ConGolog paper in IJCAI-97, and other examples designed by Hector Levesque.

In all cases, an elevator is operating in a building with six (6) floors.  The elevator can go up and down and responds to call buttons on floors being on. Once all call buttons are off, the elevator goes to the first floor and stops.

There are five versions incrementally more complex:

1. **Example 1:** the most basic case with no sensing or exogenous events.
3. **Example 2:** includes exogenous actions and sensing.

Files `main_xx.pl` are provided to start the applications; remember they need to be consulted in the context of a configuration file (defining some global variables and predicates).

## Example 1: Basic elevator - no sensing or exogenous events

There are three controllers: _basic_, _dumb_, and _smart_. You can select them interactively by querying `main/0` or directly via `main/1` (with the controller name id).

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

The _dumb_ and _smart_ controllers are meant to _minimize the number of moves of the elevator_. However, the dumb controller does not perform look-ahead search and hence fails to find a complete execution, whereas the smart controller uses look-ahead search until a full execution that solves the problem is found:

```shell
$ swipl config.pl Examples/Elevator-Vanilla/main_01.pl
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

There are two controllers:

* `basic`: This is a reactive controller, except that sensing is used to find out which elevator call lights are on at the start.
* `control`: Full fledged control using concurrency.