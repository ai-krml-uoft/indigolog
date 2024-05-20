# Action Theory Projectors



## BAT: Basic Action Theories

A basic action theory (BAT) is described with:

- `fun_fluent(F)`: defines functional fluent - could be non-ground.
- `rel_fluent(F)`: defines relational fluent - could be non-ground.
- `prim_action(A)`: defines primitive action `A` - must be ground.
- `exog_action(A)`: defines exogenous action `A` - must be ground.
- `senses(A, F)`: action `A` senses fluent `F`.
- `poss(A, C)`: precondition of action `A` is `C`.
- `initially(F, V)`: initival value of fluent `F` is `V` - has to be ground.
- `causes_val(A, F, V, C)`: functional fluent `F` takes value `V` when action `A` is executed and conditon `C` holds.
- `causes_true(A, F, C)`: relational fluent `F` becomes true when action `A` is executed and conditon `C` holds.
- `causes_false(A, F, C)`: relational fluent `F` becomes true when action `A` is executed and conditon `C` holds.
- `sort(S, domain_of_sort)`: all sorts used in the domain

        e.g., varsort(c, colors).
              varsort(temp, temperature).
              color([blue, green, yellow, red]).
              temperature([-10,0,10,20,30,40]).


 A high-level program-controller is described with:

 - `proc(P, Body)`: for each procedure `P`
 - `simulator(N, P)`: `P` is the `N` exogenous action simulator.
 - 
 The interface for Lego is described with:

 -- actionNum(action, num)
         action has RCX code num
 -- simulateSensing(action)
         sensing result for action should be asked to the user
 -- translateSensing(action, sensorValue, sensorResult)
         translate the sensorValue of action to sensorResult
 -- translateExogAction(codeAction, action)
         translateSensing action name into codeAction and vice-versa
## KBAT: Knowlwedge-based Basic Action Theories