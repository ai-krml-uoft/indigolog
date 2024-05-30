%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 - GENERAL
%
%  -- subv(+X1,+X2,+T1,-T2)
%        T2 is T1 with X1 replaced by X2
%  -- sublist(?SubList, +List)
%        Succeeds if List is the list which contains all elements from SubList
%  -- get_integer(+Low, ?N, +High)
%        N is an integer between Low and High
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

       /*  T2 is T1 with X1 replaced by X2  */
subv(X1,X2,T1,T2) :- var(T1), T1 == X1, !, T2 = X2.
subv(_,_,T1,T2)   :- var(T1), !, T2 = T1.
subv(X1,X2,T1,T2) :- T1 == X1, !, T2 = X2.
subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), T2 =..[F|L2].

subvl(_,_,[],[]).
subvl(X1,X2,[T1|L1],[T2|L2]) :- subv(X1,X2,T1,T2), subvl(X1,X2,L1,L2).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 - STRINGS AND ATOMS
%
% -- any_to_number(+T, -Number)
%       Convert an atom, string, or list of chars T into a number
% -- join_atom/3
% -- join_atom(+List, +Glue, -Atom)
%       Atom is the atom formed by concatenating the elements of List with an
%       instance of Glue beween each of them.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% -- join_atom(List, Glue, Atom)
%      Atom is the atom formed by concatenating the elements of List with an
%      instance of Glue beween each of them.
join_atom(List, Glue, Atom) :-
        maplist(any_to_string, List, List2),
        join_string(List2, Glue, String),
        string_to_atom(String, Atom).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5 - TOOL FOR REPORTING MESSAGES
%
% -- logging(+T, +M)
%       Report messsage M of type T
% -- set_debug_level(+N) : set the debug level to N (nothing >N is shown)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic
	debug_level/1,
        debug_level/2.

set_option("log_level: up to what level to report").
set_option(log_level, N) :-
        number(N),
        retractall(debug_level(_)),
	assert(debug_level(N)),
        logging(system(0), 'Debug level set to ~d', [N]).
set_option(log_level, N) :-
        N =.. [M, Level],
        retractall(debug_level(M, _)),
	assert(debug_level(M, Level)),
        logging(system(0), 'Debug level for module ~w set to ~d', [M, Level]).

% report logging using ~ formatting print
logging(T, M) :- logging(T, M, []).
logging(T, _, _) :-
        T =.. [M, Level|_],
        number(Level),
        (       debug_level(M, N), N < Level
        ->      !
        ;       debug_level(N), N < Level
        ->      !
        ;       fail
        ).
logging(T, M, L) :- \+ is_list(M), !, logging(T, [M], L).
logging(T, M, L) :-
        is_list(M), !,
        maplist(any_to_string, M, MS),
        id_logging(T, T2),
        format(atom(Header), "~w:", [T2]),
	atomics_to_string([Header|MS], " ", Message),
        format(Message, L), nl.

id_logging(T, TS) :-
        term_string(T, T2),
        string_upper(T2, TS).



% Deal with an unknown configuration (P, H)
log_error(M) :-
        logging(error, M),
        logging(error, "Execution will be aborted!"), abort.

log_warn(M) :-
        logging(warning, M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4 - OTHER TOOLS
%
% -- turn_on_gc/0
% -- turn_off_gc/0
%       Turn on/off garbage collection
% -- set_backquoted_string/0
%       Set the backquoted_string flag to true (transparent predicate)
% -- catch_succ(+Call,+Message)
% -- catch_fail(+Call,+Message)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Turn on/off the automatic garbage collector
turn_on_gc  :- set_prolog_flag(gc, true).
turn_off_gc :- set_prolog_flag(gc, false).



% Perform a call catching it if there is an exception
catch_call(Goal, Message) :- catch_call(Goal, Message, true).
catch_call(Goal, Message, Goal2) :-
	catch(Goal, E, (logging(warning, "~w ---> ~w", [Message, E]), Goal2)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

