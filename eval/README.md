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
- `causes_true(A, F, C)`: relational fluent `F` becomes true when action `A` is executed and condition `C` holds.
- `causes_false(A, F, C)`: relational fluent `F` becomes true when action `A` is executed and condition `C` holds.


## KBAT: Knowlwedge-based Basic Action Theories