%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE    : lib/alpha_star.pl
%
%    Abstract A* Algorithm Variation
%
%    DESCRIPTION:    An A* path finding implementation for use with
%                    indigolog programs.
%    ORIGINAL:	     http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/5_1.html
%    LAST REVISED:	 Stavros Vassos (March 8st, 2005)
%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%    MAIN PREDICATE: pathfind(+Start, +End, -Solution)
%    NEEDS:          pathfind_move(+Start, -End, -Action) 
%                       pathfind_move/3 specifies what moves can be achieved 
%                       and where they leed to
%                    pathfind_heuristic(+State,+Goal,-H)
%                       pathfind_heuristic/3 specifies a heuristic metric needed
%                       for prioritizing the possible actions
%    
%    NOTES:          - pathfind/3 only finds one solution and does not backtrack.
%                    - As it is now, it uses a dynamic predicate pathf/1 to
%                    keep track of visited locations. This should be 
%                    replaced by a list which is passed along the predicates.
%                    - All predicates in this file have the prefix "pathfind" so
%                    that there is no conflict with any other library.
%    
%    NOTES/DESCRIPTION FROM ORIGINAL:
%     Nodes have form    S#D#F#A
%            where S describes the state or configuration
%                  D is the depth of the node
%                  F is the evaluation function value
%                  A is the ancestor list for the node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(400,yfx,'#').    /* Node builder notation */
%:- dynamic(pathf/1).

pathfind(State,Goal,Soln) :- 
		pathfind_f_function(State,Goal,0,F),
		%retractall(pathf(_)),
		pathfind_search([State#0#F#[]],Goal,S), reverse(S,Soln).

pathfind_f_function(State,Goal,D,F) :- pathfind_heuristic(State,Goal,H), F is D + H.

pathfind_search([State#_#_#Soln|_], State, Soln).
pathfind_search([B|R],Goal,S) :- 
		%B = State#_#_#_,
		%assert(pathf(State)),
		pathfind_expand(B,Goal,Children),
		pathfind_insert_all(Children,R,Open),
		pathfind_search(Open,Goal,S).

pathfind_insert_all([F|R],Open1,Open3) :- 
		pathfind_insert(F,Open1,Open2),
		pathfind_insert_all(R,Open2,Open3).
pathfind_insert_all([],Open,Open).

pathfind_insert(B,Open,Open) :- pathfind_repeat_node(B,Open), ! .
pathfind_insert(B,[C|R],[B,C|R]) :- pathfind_cheaper(B,C), ! .
pathfind_insert(B,[B1|R],[B1|S]) :- pathfind_insert(B,R,S), !.
pathfind_insert(B,[],[B]).

pathfind_repeat_node(P#_#_#_, [P#_#_#_|_]).

pathfind_cheaper( _#_#F1#_ , _#_#F2#_ ) :- F1 < F2.

pathfind_expand(State#D#_#S,Goal,All_My_Children) :-
     bagof(Child#D1#F#[Move|S],
			(D1 is D+1,
			pathfind_move(State,Child,Move),
			%\+ pathf(Child),
			pathfind_f_function(Child,Goal,D1,F)),
           	All_My_Children).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/alpha_star.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%