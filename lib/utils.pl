:- module(utils, [
     % 1 - TOOLS
     subv/4,
     print_prog/1,
     print_prog/2,
     % 2 - DATA CONVERSION
     any_to_string/2,
     join_atom/3,
     % 3 - OS TOOLS
     send_term/2
          ]).


%!      subv(++X2, ++X1, +T1, -T2) is det
%       T2 is T1 with X1 replaced by X2 - T2 = T1|_{X1/X2}
subv(X1, X2, T1, T2) :- var(T1), T1 == X1, !, T2 = X2.
subv(_, _, T1, T2)   :- var(T1), !, T2 = T1.
subv(X1, X2, T1, T2) :- T1 == X1, !, T2 = X2.
subv(X1, X2, T1, T2) :- T1 =..[F|L1], maplist(subv(X1, X2), L1, L2), T2 =..[F|L2].


%!      any_to_string(+A, -S) is det
% Convert anything A into a string S.
%    OBS: generalizes term_string/2 to allow variables
any_to_string(A, "_Var") :- var(A), !.
any_to_string(A, A) :- string(A), !.
any_to_string(A, S) :- atom(A), atom_string(A, S), !.
any_to_string(A, S) :- term_string(A, S).


%!      join_atom(List, Glue, Atom)
%    Atom is the atom formed by concatenating the elements of List with an
%      instance of Glue between each of them.
join_atom(List, Glue, Atom) :-
        maplist(any_to_string, List, List2),
        join_string(List2, Glue, String),
        string_to_atom(String, Atom).


% this will send the term, full stop, and a space
send_term(Stream, Term) :-
     write_term(Stream, Term, [quoted(true), fullstop(true), nl(false), spacing(next_argument)]),
     flush_output(Stream).


%!        print_prog(E) is det
%    pretty print a rogram with identation
print_prog(E) :- print_term(E, [auto_indent_arguments(2)]).
print_prog(E, N) :- print_term(E, [auto_indent_arguments(N)]).