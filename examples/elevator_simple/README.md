# Vanilla Elevator Controllers

This folder contains give versions of a simple elevator controller for IndiGolog that appeared in Reiter's 2001 Knowledge in Action book, the ConGolog paper in IJCAI-97, and other examples designed by Hector Levesque.

In all cases, an elevator is operating in a building with six (6) floors.  The elevator can go up and down and responds to call buttons on floors being on. Once all call buttons are off, the elevator goes to the first floor and stops.

There are five versions incrementally more complex:

1. **Example 1:** the most basic case with no sensing or exogenous events.
3. **Example 2:** has exogenous actions that can change the world.
4. **Example 4:** has sensing of individual elevator buttons.
5. **Example 5:** has sensing of all elevator buttons.

## Example 1: Basic elevator - no sensing or exogenous events

There are three controllers: _basic_, _dumb_, and _smart_. You can select them interactively by querying `main/0` or directly via `main/2`.

The basic controller just picks pending floors randomly one by one until all are served, then park at floor 1.

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

The _dumb_ and _smart_ are meant to minimize the number of moves of the elevator. However, the dumb controller does not perform lookeahed search and hence fails to find a complete execution, whereas the smart controller uses lookeahead search until a full execution that solves the problem is found:

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

