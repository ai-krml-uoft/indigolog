:- module(utils, [
     % 1 - TOOLS
     % 2 - STRINGS
     any_to_string/2,
     % 3 - OS TOOLS
     send_term/2
          ]).



% Convert anything into a string: generalizes term_string/2 to allow vars
any_to_string(A, "_Var") :- var(A), !.
any_to_string(A, A) :- string(A), !.
any_to_string(A, S) :- atom(A), atom_string(A, S), !.
any_to_string(A, S) :- term_string(A, S).



% this will send the term, full stop, and a space
send_term(Stream, Term) :-
     write_term(Stream, Term, [quoted(true), fullstop(true), nl(false), spacing(next_argument)]),
     flush_output(Stream).

