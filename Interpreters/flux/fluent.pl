
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- lib(fd).
neq(_6653, _6656) :- or_neq(exists, _6653, _6656).
neq_all(_6681, _6684) :- or_neq(forall, _6681, _6684).
or_neq(_6709, _6712, _6715) :- _6712 =.. [_6728|_6729], _6715 =.. [_6738|_6739], (_6728 = _6738 -> or_neq(_6709, _6729, _6739, _6757), call(_6757) ; true).
or_neq(_6928, [], [], 0 #\= 0).
or_neq(_6971, [_6976|_6977], [_6982|_6983], _6986) :- or_neq(_6971, _6977, _6983, _7003), (_6971 = forall, var(_6976) -> _6986 = _7003 ; _6986 = #\/(_6976 #\= _6982, _7003)).
and_eq([], [], 0 #= 0).
and_eq([_7246|_7247], [_7252|_7253], _7256) :- and_eq(_7247, _7253, _7270), _7256 = #/\(_7246 #= _7252, _7270).
or_and_eq([], 0 #\= 0).
or_and_eq([eq(_7430, _7433)|_7437], #\/(_7440, _7444)) :- or_and_eq(_7437, _7440), and_eq(_7430, _7433, _7444).
member(_7559, [_7559|_7564], _7564).
member(_7579, [_7584|_7585], [_7584|_7590]) :- member(_7579, _7585, _7590).
cancel(_7616, _7619, _7622) :- var(_7619) -> cancel(_7616, _7619), cancelled(_7616, _7619), _7622 = _7619 ; _7619 = [_7668|_7669], (_7616 \= _7668 -> cancel(_7616, _7669, _7685), _7622 = [_7668|_7685] ; cancel(_7616, _7669, _7622)).
not_holds(A, B) :-
	'CHRnot_holds_2'(not_holds(A, B), C, D, E).



%%% Rules handling for not_holds / 2

'CHRnot_holds_2'(not_holds(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRnot_holds_2'(not_holds(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRnot_holds_2'(not_holds(A, [B|C]), D, E, F) ?-
	!,
	D = true,
	neq(A, B),
	not_holds(A, C).
'CHRnot_holds_2'(not_holds(A, []), B, C, D) ?-
	!,
	B = true.
'CHRnot_holds_2'(not_holds(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_2__23'(F, [B], [G], H),
	no_delayed_goals(instance(A, G)),
	!,
	C = true.
'CHRnot_holds_2'(not_holds(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_2__24'(F, [B], [G], H),
	no_delayed_goals(\+ G \= A),
	!,
	C = true,
	cancel(G, B).
'CHRnot_holds_2'(not_holds(A, B), C, D, E) :-
	'CHRnot_holds_2__22'(not_holds(A, B), C, D, E).
'CHRnot_holds_2__23'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRnot_holds_2__23'([A|B], C, D, E) :-
	'CHRnot_holds_2__23'(B, C, D, E).
:- set_flag('CHRnot_holds_2__23' / 4, leash, notrace).
'CHRnot_holds_2__24'(['CHRcancel_2'(cancel(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRnot_holds_2__24'([A|B], C, D, E) :-
	'CHRnot_holds_2__24'(B, C, D, E).
:- set_flag('CHRnot_holds_2__24' / 4, leash, notrace).
:- set_flag('CHRnot_holds_2' / 4, leash, notrace).
:- current_macro('CHRnot_holds_2' / 4, _9445, _9446, _9447) -> true ; define_macro('CHRnot_holds_2' / 4, tr_chr / 2, [write]).
'CHRnot_holds_2__22'(A, B, C, D) :-
	'CHRnot_holds_2__25'(A, B, C, D).
:- set_flag('CHRnot_holds_2__22' / 4, leash, notrace).
'CHRnot_holds_2__25'(not_holds(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_2__25__26'(F, C, not_holds(A, B), D, E).
'CHRnot_holds_2__25'(not_holds(A, B), C, D, E) :-
	'CHRnot_holds_2__25__27'(not_holds(A, B), C, D, E).
:- set_flag('CHRnot_holds_2__25' / 4, leash, notrace).
'CHRnot_holds_2__25__26'(['CHRor_2'(or(A, B), C, D, E)|F], G, not_holds(H, B), I, J) ?-
	var(C),
	no_delayed_goals((member(K, A, L), H == K)),
	!,
	C = true,
	'CHRnot_holds_2__25__26'(F, G, not_holds(H, B), I, J),
	or(L, B).
'CHRnot_holds_2__25__26'([A|B], C, D, E, F) :-
	'CHRnot_holds_2__25__26'(B, C, D, E, F).
'CHRnot_holds_2__25__26'([], A, B, C, D) :-
	'CHRnot_holds_2__25__27'(B, A, C, D).
:- set_flag('CHRnot_holds_2__25__26' / 5, leash, notrace).
'CHRnot_holds_2__25__27'(not_holds(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, not_holds(A, B)], 'CHRnot_holds_2'(not_holds(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRnot_holds_2__25__27' / 4, leash, notrace).
not_holds_all(A, B) :-
	'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E).



%%% Rules handling for not_holds_all / 2

'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRnot_holds_all_2'(not_holds_all(A, [B|C]), D, E, F) ?-
	!,
	D = true,
	neq_all(A, B),
	not_holds_all(A, C).
'CHRnot_holds_all_2'(not_holds_all(A, []), B, C, D) ?-
	!,
	B = true.
'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__29'(F, [B], [G], H),
	no_delayed_goals(instance(A, G)),
	!,
	C = true.
'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__28'(not_holds_all(A, B), C, D, E).
'CHRnot_holds_all_2__29'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRnot_holds_all_2__29'([A|B], C, D, E) :-
	'CHRnot_holds_all_2__29'(B, C, D, E).
:- set_flag('CHRnot_holds_all_2__29' / 4, leash, notrace).
:- set_flag('CHRnot_holds_all_2' / 4, leash, notrace).
:- current_macro('CHRnot_holds_all_2' / 4, _11111, _11112, _11113) -> true ; define_macro('CHRnot_holds_all_2' / 4, tr_chr / 2, [write]).
'CHRnot_holds_all_2__28'(A, B, C, D) :-
	'CHRnot_holds_all_2__30'(A, B, C, D).
:- set_flag('CHRnot_holds_all_2__28' / 4, leash, notrace).
'CHRnot_holds_all_2__30'(not_holds_all(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__30__31'(F, C, not_holds_all(A, B), D, E).
'CHRnot_holds_all_2__30'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__30__32'(not_holds_all(A, B), C, D, E).
:- set_flag('CHRnot_holds_all_2__30' / 4, leash, notrace).
'CHRnot_holds_all_2__30__31'(['CHRnot_holds_2'(not_holds(A, B), C, D, E)|F], G, not_holds_all(H, B), I, J) ?-
	var(C),
	no_delayed_goals(instance(A, H)),
	!,
	C = true,
	'CHRnot_holds_all_2__30__31'(F, G, not_holds_all(H, B), I, J).
'CHRnot_holds_all_2__30__31'([A|B], C, D, E, F) :-
	'CHRnot_holds_all_2__30__31'(B, C, D, E, F).
'CHRnot_holds_all_2__30__31'([], A, B, C, D) :-
	'CHRnot_holds_all_2__30__32'(B, A, C, D).
:- set_flag('CHRnot_holds_all_2__30__31' / 5, leash, notrace).
'CHRnot_holds_all_2__30__32'(not_holds_all(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__30__32__33'(F, C, not_holds_all(A, B), D, E).
'CHRnot_holds_all_2__30__32'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__30__32__34'(not_holds_all(A, B), C, D, E).
:- set_flag('CHRnot_holds_all_2__30__32' / 4, leash, notrace).
'CHRnot_holds_all_2__30__32__33'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], G, not_holds_all(H, B), I, J) ?-
	var(C),
	no_delayed_goals(instance(A, H)),
	!,
	C = true,
	'CHRnot_holds_all_2__30__32__33'(F, G, not_holds_all(H, B), I, J).
'CHRnot_holds_all_2__30__32__33'([A|B], C, D, E, F) :-
	'CHRnot_holds_all_2__30__32__33'(B, C, D, E, F).
'CHRnot_holds_all_2__30__32__33'([], A, B, C, D) :-
	'CHRnot_holds_all_2__30__32__34'(B, A, C, D).
:- set_flag('CHRnot_holds_all_2__30__32__33' / 5, leash, notrace).
'CHRnot_holds_all_2__30__32__34'(not_holds_all(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRnot_holds_all_2__30__32__34__35'(F, C, not_holds_all(A, B), D, E).
'CHRnot_holds_all_2__30__32__34'(not_holds_all(A, B), C, D, E) :-
	'CHRnot_holds_all_2__30__32__34__36'(not_holds_all(A, B), C, D, E).
:- set_flag('CHRnot_holds_all_2__30__32__34' / 4, leash, notrace).
'CHRnot_holds_all_2__30__32__34__35'(['CHRor_2'(or(A, B), C, D, E)|F], G, not_holds_all(H, B), I, J) ?-
	var(C),
	no_delayed_goals((member(K, A, L), instance(K, H))),
	!,
	C = true,
	'CHRnot_holds_all_2__30__32__34__35'(F, G, not_holds_all(H, B), I, J),
	or(L, B).
'CHRnot_holds_all_2__30__32__34__35'([A|B], C, D, E, F) :-
	'CHRnot_holds_all_2__30__32__34__35'(B, C, D, E, F).
'CHRnot_holds_all_2__30__32__34__35'([], A, B, C, D) :-
	'CHRnot_holds_all_2__30__32__34__36'(B, A, C, D).
:- set_flag('CHRnot_holds_all_2__30__32__34__35' / 5, leash, notrace).
'CHRnot_holds_all_2__30__32__34__36'(not_holds_all(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, not_holds_all(A, B)], 'CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRnot_holds_all_2__30__32__34__36' / 4, leash, notrace).
duplicate_free(A) :-
	'CHRduplicate_free_1'(duplicate_free(A), B, C, D).



%%% Rules handling for duplicate_free / 1

'CHRduplicate_free_1'(duplicate_free(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRduplicate_free_1'(duplicate_free(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRduplicate_free_1'(duplicate_free([A|B]), C, D, E) ?-
	!,
	C = true,
	not_holds(A, B),
	duplicate_free(B).
'CHRduplicate_free_1'(duplicate_free([]), A, B, C) ?-
	!,
	A = true.
'CHRduplicate_free_1'(duplicate_free(A), B, C, D) :-
	'CHRduplicate_free_1__37'(duplicate_free(A), B, C, D).
:- set_flag('CHRduplicate_free_1' / 4, leash, notrace).
:- current_macro('CHRduplicate_free_1' / 4, _13346, _13347, _13348) -> true ; define_macro('CHRduplicate_free_1' / 4, tr_chr / 2, [write]).
'CHRduplicate_free_1__37'(A, B, C, D) :-
	'CHRduplicate_free_1__38'(A, B, C, D).
:- set_flag('CHRduplicate_free_1__37' / 4, leash, notrace).
'CHRduplicate_free_1__38'(duplicate_free(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, duplicate_free(A)], 'CHRduplicate_free_1'(duplicate_free(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRduplicate_free_1__38' / 4, leash, notrace).
or(A, B) :-
	'CHRor_2'(or(A, B), C, D, E).



%%% Rules handling for or / 2

'CHRor_2'(or(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRor_2'(or(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRor_2'(or([eq(A, B)], C), D, E, F) ?-
	!,
	D = true,
	and_eq(A, B, G),
	call(G).
'CHRor_2'(or([A], B), C, D, E) ?-
	!,
	C = true,
	holds(A, B).
'CHRor_2'(or(A, B), C, D, E) ?-
	no_delayed_goals(\+ (member(F, A), F \= eq(G, H))),
	!,
	C = true,
	or_and_eq(A, I),
	call(I).
'CHRor_2'(or(A, []), B, C, D) ?-
	no_delayed_goals((member(E, A, F), E \= eq(G, H))),
	!,
	B = true,
	or(F, []).
'CHRor_2'(or(A, B), C, D, E) ?-
	no_delayed_goals((member(eq(F, G), A), or_neq(exists, F, G, H), \+ call(H))),
	!,
	C = true.
'CHRor_2'(or(A, B), C, D, E) ?-
	no_delayed_goals((member(eq(F, G), A, H), \+ (and_eq(F, G, I), call(I)))),
	!,
	C = true,
	or(H, B).
'CHRor_2'(or(A, [B|C]), D, E, F) ?-
	!,
	D = true,
	or(A, [], [B|C]).
'CHRor_2'(or(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRor_2__40'(F, [B], [G], H),
	no_delayed_goals((member(I, A, J), G == I)),
	!,
	C = true,
	or(J, B).
'CHRor_2'(or(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRor_2__41'(F, [B], [G], H),
	no_delayed_goals((member(I, A, J), instance(I, G))),
	!,
	C = true,
	or(J, B).
'CHRor_2'(or(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRor_2__42'(F, [B], [G], H),
	no_delayed_goals((member(I, A), \+ G \= I)),
	!,
	C = true,
	cancel(G, B).
'CHRor_2'(or(A, B), C, D, E) :-
	'CHRor_2__39'(or(A, B), C, D, E).
'CHRor_2__40'(['CHRnot_holds_2'(not_holds(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRor_2__40'([A|B], C, D, E) :-
	'CHRor_2__40'(B, C, D, E).
:- set_flag('CHRor_2__40' / 4, leash, notrace).
'CHRor_2__41'(['CHRnot_holds_all_2'(not_holds_all(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRor_2__41'([A|B], C, D, E) :-
	'CHRor_2__41'(B, C, D, E).
:- set_flag('CHRor_2__41' / 4, leash, notrace).
'CHRor_2__42'(['CHRcancel_2'(cancel(A, B), C, D, E)|F], [B], [G], H) ?-
	var(C),
	[A, E] = [G, H].
'CHRor_2__42'([A|B], C, D, E) :-
	'CHRor_2__42'(B, C, D, E).
:- set_flag('CHRor_2__42' / 4, leash, notrace).
:- set_flag('CHRor_2' / 4, leash, notrace).
:- current_macro('CHRor_2' / 4, _16249, _16250, _16251) -> true ; define_macro('CHRor_2' / 4, tr_chr / 2, [write]).
'CHRor_2__39'(A, B, C, D) :-
	'CHRor_2__43'(A, B, C, D).
:- set_flag('CHRor_2__39' / 4, leash, notrace).
'CHRor_2__43'(or(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, or(A, B)], 'CHRor_2'(or(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRor_2__43' / 4, leash, notrace).
or(A, B, C) :-
	'CHRor_3'(or(A, B, C), D, E, F).



%%% Rules handling for or / 3

'CHRor_3'(or(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRor_3'(or(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRor_3'(or(A, B, [C|D]), E, F, G) ?-
	no_delayed_goals((member(H, A, I), \+ C \= H)),
	!,
	E = true,
	(
	    H == C
	->
	    true
	;
	    H =.. [J|K],
	    C =.. [L|M],
	    or(I, [eq(K, M), H|B], [C|D])
	).
'CHRor_3'(or(A, B, [C|D]), E, F, G) ?-
	!,
	E = true,
	append(A, B, H),
	or(H, D).
'CHRor_3'(or(A, B, C), D, E, F) :-
	'CHRor_3__44'(or(A, B, C), D, E, F).
:- set_flag('CHRor_3' / 4, leash, notrace).
:- current_macro('CHRor_3' / 4, _17107, _17108, _17109) -> true ; define_macro('CHRor_3' / 4, tr_chr / 2, [write]).
'CHRor_3__44'(A, B, C, D) :-
	'CHRor_3__45'(A, B, C, D).
:- set_flag('CHRor_3__44' / 4, leash, notrace).
'CHRor_3__45'(or(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, or(A, B, C)], 'CHRor_3'(or(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRor_3__45' / 4, leash, notrace).
cancel(A, B) :-
	'CHRcancel_2'(cancel(A, B), C, D, E).



%%% Rules handling for cancel / 2

'CHRcancel_2'(cancel(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRcancel_2'(cancel(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRcancel_2'(cancel(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRcancel_2__47'(F, [B, A], [], G),
	!,
	C = true.
'CHRcancel_2'(cancel(A, B), C, D, E) :-
	'CHRcancel_2__46'(cancel(A, B), C, D, E).
'CHRcancel_2__47'(['CHRcancelled_2'(cancelled(A, B), C, D, E)|F], [B, A], [], G) ?-
	var(C),
	[C, E] = [true, G].
'CHRcancel_2__47'([A|B], C, D, E) :-
	'CHRcancel_2__47'(B, C, D, E).
:- set_flag('CHRcancel_2__47' / 4, leash, notrace).
:- set_flag('CHRcancel_2' / 4, leash, notrace).
:- current_macro('CHRcancel_2' / 4, _17969, _17970, _17971) -> true ; define_macro('CHRcancel_2' / 4, tr_chr / 2, [write]).
'CHRcancel_2__46'(A, B, C, D) :-
	'CHRcancel_2__48'(A, B, C, D).
:- set_flag('CHRcancel_2__46' / 4, leash, notrace).
'CHRcancel_2__48'(cancel(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRcancel_2__48__49'(F, C, cancel(A, B), D, E).
'CHRcancel_2__48'(cancel(A, B), C, D, E) :-
	'CHRcancel_2__48__50'(cancel(A, B), C, D, E).
:- set_flag('CHRcancel_2__48' / 4, leash, notrace).
'CHRcancel_2__48__49'(['CHRnot_holds_2'(not_holds(A, B), C, D, E)|F], G, cancel(H, B), I, J) ?-
	var(C),
	no_delayed_goals(\+ H \= A),
	!,
	C = true,
	'CHRcancel_2__48__49'(F, G, cancel(H, B), I, J),
	cancel(H, B).
'CHRcancel_2__48__49'([A|B], C, D, E, F) :-
	'CHRcancel_2__48__49'(B, C, D, E, F).
'CHRcancel_2__48__49'([], A, B, C, D) :-
	'CHRcancel_2__48__50'(B, A, C, D).
:- set_flag('CHRcancel_2__48__49' / 5, leash, notrace).
'CHRcancel_2__48__50'(cancel(A, B), C, D, E) ?-
	var(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRcancel_2__48__50__51'(F, C, cancel(A, B), D, E).
'CHRcancel_2__48__50'(cancel(A, B), C, D, E) :-
	'CHRcancel_2__48__50__52'(cancel(A, B), C, D, E).
:- set_flag('CHRcancel_2__48__50' / 4, leash, notrace).
'CHRcancel_2__48__50__51'(['CHRor_2'(or(A, B), C, D, E)|F], G, cancel(H, B), I, J) ?-
	var(C),
	no_delayed_goals((member(K, A), \+ H \= K)),
	!,
	C = true,
	'CHRcancel_2__48__50__51'(F, G, cancel(H, B), I, J),
	cancel(H, B).
'CHRcancel_2__48__50__51'([A|B], C, D, E, F) :-
	'CHRcancel_2__48__50__51'(B, C, D, E, F).
'CHRcancel_2__48__50__51'([], A, B, C, D) :-
	'CHRcancel_2__48__50__52'(B, A, C, D).
:- set_flag('CHRcancel_2__48__50__51' / 5, leash, notrace).
'CHRcancel_2__48__50__52'(cancel(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, cancel(A, B)], 'CHRcancel_2'(cancel(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRcancel_2__48__50__52' / 4, leash, notrace).
cancelled(A, B) :-
	'CHRcancelled_2'(cancelled(A, B), C, D, E).



%%% Rules handling for cancelled / 2

'CHRcancelled_2'(cancelled(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRcancelled_2'(cancelled(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRcancelled_2'(cancelled(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRcancelled_2__54'(F, [B, A], [], G),
	!,
	C = true.
'CHRcancelled_2'(cancelled(A, B), C, D, E) :-
	'CHRcancelled_2__53'(cancelled(A, B), C, D, E).
'CHRcancelled_2__54'(['CHRcancel_2'(cancel(A, B), C, D, E)|F], [B, A], [], G) ?-
	var(C),
	[C, E] = [true, G].
'CHRcancelled_2__54'([A|B], C, D, E) :-
	'CHRcancelled_2__54'(B, C, D, E).
:- set_flag('CHRcancelled_2__54' / 4, leash, notrace).
:- set_flag('CHRcancelled_2' / 4, leash, notrace).
:- current_macro('CHRcancelled_2' / 4, _19855, _19856, _19857) -> true ; define_macro('CHRcancelled_2' / 4, tr_chr / 2, [write]).
'CHRcancelled_2__53'(A, B, C, D) :-
	'CHRcancelled_2__55'(A, B, C, D).
:- set_flag('CHRcancelled_2__53' / 4, leash, notrace).
'CHRcancelled_2__55'(cancelled(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, cancelled(A, B)], 'CHRcancelled_2'(cancelled(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRcancelled_2__55' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
