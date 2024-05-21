:- module(utils, [
        join_to_string/2,
        join_to_string/3,
        any_to_string/2,
        logging/2,
        logging/3
          ]).


% join_to_string(+List, +Sep, -String):
%    String is concatenated elements of List separated by Sep
%   generalization of https://www.swi-prolog.org/pldoc/man?predicate=atomics_to_string/2
%   to handle terms
join_to_string(L, String) :- join_string(L, "", String).
join_to_string(L, Sep, String) :- maplist(term_string, L, L2), atomics_to_string(L2, Sep, String).


% Convert anything into a string: generalizes term_string/2 to allow vars
any_to_string(A, "_Var") :- var(A), !.
any_to_string(A, S) :- term_string(A, S).



