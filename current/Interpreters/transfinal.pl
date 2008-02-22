%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/transfinal.pl
%
%       IndiGolog TRANS & FINAL Implementation (Version 5)
%
%  AUTHOR : Sebastian Sardina 
%           based on the definitions for ConGolog by 
%		Giuseppe De Giaccomo, Yves Lesperance, and Hector Levesque
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%    This file contains the definition of TRANS and FINAL for all the 
%	constructs in the language
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file provides:
%
% -- trans(P,H,P2,H2)    : configuration (P,H) can perform a single step
%                          to configuration (P2,H2)
% -- final(P,H)          : configuration (P,H) is terminating
%
% -- do(P,H,H')          : Golog Do/3 using search(E)
%
%  The following special features are also provided:
% 
% -- A special action `wait' that is always possible. It can be used to state 
%         that the program will be waiting for an exogenous action.
% -- A special action `abort' that is always possible. It can be used to state 
%         that the program should fail.
% -- A special action `sim(E)' for each exogenous action E. The action is
%         always possible and it is used to assume the occurrence of E
% -- A special action `stop_interrupts' that is used to set the fluent
%         interrupts_running to false
% -- A special fluent `interrupts_running' that is always true unless stopped
%
%
%  The following is required for this file:
%
% -- wscp(Name,G,Max,IA,SimNo,H,E): WSCP implementation
%        Name : name of the planning problem
%        G    : goal to be achieved
%        IA   : initial list of possible actions
%        SimNo: identification of the exogenous action simulator to be used
%        H    : Initial situation-history
%        E    : SOLUTION: CONDITIONAL PLAN
%
% FROM SYSTEM CODE DEPENDING ON WHERE IT IS USED (hookvir.pl or hookrxc.pl)
% -- unknown(P,H): TRANS or FINAL for (P,H) is unknown 
%                  (some condition is unknown to be true or false)
% -- report_message(T, M) : report message M of type T
%
% FROM TEMPORAL PROJECTOR:
% -- eval(+C, +H, -B) 
%           B is the truth value of C at history H
% -- calc_arg(+A, -A2, +H) 
%           calculate the arguments of action A at history H
% -- domain(-V, +D)       
% -- rdomain(-V, +D)       
%           object V is an element of domain D (random)
% -- getdomain(+D, -L) 
%           L is the list of elements in domain D
% -- sensed(+A, ?V, ?H) 
%           action A got sensing result V w.r.t. history H
% -- inconsistent(H) 
%           last action make history H inconsistent, i.e. impossible 
% -- assume(F, V, H1, H2) 
%           H2 is the history resulting from assuming fluent F
%           to have value V at history H1
% -- before(+H1, +H2)
%           history H1 is a prefix of H2
%
% FROM DOMAIN SPECIFIC CODE:
% -- prim_action(action) : for each primitive action 
% -- exog_action(action) : for each exogenous action 
% -- poss(action,cond)   : precondition axioms
%
%  Code for describing the high-level program:
% -- proc(name,P)           : for each procedure P 
% -- simulator(N,P)         : P is the N exogenous action simulator
%
% OTHERS (PROLOG SPECIFIC):
% -- false
%            equivalent to fail
% -- random(+L,+U,-R) 
%            randomly returns a number R between L and U
% -- subv(+X1,+X2,+T1,-T2) 
%            T2 is T1 with X1 replaced by X2
% -- catch/3 and throw/1 for handling exceptions 
%            (provide empty implementations of both if there is no support 
%            for exceptions available)
% -- shuffle/2 : shuffle a list into another list in a random way
%
% -- call_with_time_limit(+Sec, +Goal): True if Goal completes within Time. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            TRANS and FINAL                           
%% Trans(E,H,E1,H1) ->  One execution step of program E from history H  
%%			 leads to program E1 with history H1.           
%% Final(E,H)       ->  Program E at history H is final.                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    /* (A) EXTENDED CONSTRUCTS                                           */
    /*    wndet(E1,E2) : Weak nondeterministic choice of program         */
    /*    rndet(E1,E2) : Real nondeterministic choice of program	 */
    /*    rconc(E1,E2) : Real concurrency on 2 programs   	    	 */
    /*    rconc(L) 	   : Real concurrency on a list of programs L 	 */
    /*    itconc(L)	   : Real interative concurrency on list of programs L 	 */
    /*    rpi(X,D)     : Real nondeterministic choice of argument from D */
    /*    gexec(P,E)   : Guarded execution of program E wrt condition P  */
    /*    goal(PSucc,E,PFail,ERec): full guarded execution		 */
    /*    abort(P)     : Abort process identified with P                 */
    /*    ??(P)        : Like ?(P) but it leaves a test(P) mark in H     */
    /*    wait         : Meta action to wait until an exogenous event    */
    /*    commit       : Meta action to commit to the plan found so far  */
    /*    abort        : Meta action to, suddenly,  abort execution      */
    /*	  time(P,Sec)  : Make first step on P in less than Sec seconds	 */
    /*	  ttime(P,Sec) : Make every step on P in less than Sec seconds 	 */
% Try to execute program E1 first. If impossible, then try program E2 instead
trans(wndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) -> true ; trans(E2,H,E,H1).
final(wndet(E1,E2),H)      :- final(E1,H), final(E2,H).
%final(wndet(E1,E2),H)      :- final(E1,H) ; (\+ trans(E1,H,_,H), final(E2,H)).

% Simulate random choice in a nondeterministc choice of programs
trans(rndet(E1,E2),H,E,H1):- 
        random(1,10,R), % flip a coin!
	(R>5 -> (trans(E1,H,E,H1) ; trans(E2,H,E,H1)) ;
	        (trans(E2,H,E,H1) ; trans(E1,H,E,H1)) ).
final(rndet(E1,E2),H):- final(E1,H) ; final(E2,H).

% Simulate random choice in a concurrent execution of E1 and E2
trans(rconc(E1,E2),H,rconc(E11,E22),H1) :- 
    (random(1, 3, 1) -> 	% flip a coin!
    	( (trans(E1,H,E11,H1), E22=E2) ; (trans(E2,H,E22,H1), E11=E1) ) 
    	;
        ( (trans(E2,H,E22,H1), E11=E1) ; (trans(E1,H,E11,H1), E22=E2) ) 
    ).
trans(rconc(L),H,rconc([E1|LRest]),H1) :-
	length(L,LL),
	random(0,LL, R),
	nth0(R,L,E),
	trans(E,H,E1,H1),
	select(E,L,LRest).	 
final(rconc(E1,E2),H) :- final(conc(E1,E2),H).
final(rconc([]),_).
final(rconc([E|L]),H) :- final(E,H), final(rconc(L),H).

trans(itconc(L),H,itconc(L2),H1) :-
	select(E,L,LRest),
	trans(E,H,E1,H1),
	append(LRest,[E1],L2).
final(itconc(L),H) :- final(rconc(L),H).



% Execute program E as long as condition P holds; finish E if neg(P) holds
final(gexec(_,E), H) :- final(E,H).
trans(gexec(P,E), H, gexec2(P,E1), H1) :- 	% P needs to be a simple fluent
        assume(P, true, H, H2),    % Set P to be TRUE
        trans(E, H2, E1, H1).
final(gexec2(P,E), H) :- isTrue(neg(P),H) ; final(E,H).
trans(gexec2(P,E), H, gexec2(P,E1), H1) :- isTrue(P,H), trans(E,H,E1,H1).


% goal(PSucc,E,PFail,ERec): full guarded execution
%	PSucc 	: finalize successfully if PSucc holds
%	E	: the program to be executed
%	PFail	: Terminate the program E and execute recovery procedure ERec
final(goal(PSucc,E,_,_), H) :- isTrue(PSucc,H) ; final(E,H).
trans(goal(PSucc,_,PFail,ERec), H, E2, H2) :-
	isTrue(neg(PSucc),H),
	isTrue(PFail,H),
	trans(ERec,H, E2, H2).
trans(goal(PSucc,E,PFail,ERec), H, goal(PSucc,E2,PFail,ERec), H2) :-
	isTrue(neg(PSucc),H),
	isTrue(neg(PFail),H),
	trans(E,H,E2,H2).

% Abort process identified with P by setting P to false in H
trans(abort(P), H, [], H1) :- assume(P, false, H, H1).

% Perform program P(V) with all elements in domain D: P(e1);P(e2);...;P(en)
% Domain D can either be a list of elements or a domain identifier from 
% where we get its domain list with getdomain/2
trans(for(V,D,P),H,E1,H1) :- D\=[], atom(D), !, getdomain(D,L), 
                             trans(for(V,L,P),H,E1,H1).
trans(for(V,[F|L],P),H,[E1,for(V,L,P)],H1) :- 
	subv(V,F,P,P1), trans(P1,H,E1,H1).

final(for(V,D,P),H)    :- D\=[], atom(D), !, getdomain(D,L), 
                          final(for(V,L,P),H).
final(for(_,[],_),_).
final(for(V,[F|L],P),H):- subv(V,F,P,P1), final(P1,H), final(for(V,L,P),H).

% A test action that leaves a mark in the history
trans(??(P),H,[],[test(P)|H]):- isTrue(P,H). 

% Simulation of exogenous actions E
trans(sim(E),H,[],[sim(E)|H]):- !, calc_arg(E,E1,H), exog_action(E1).

% Wait and commit are two "meta" actions.
% wait action tells the interpreter to wait until an exogenous action arrives
% commit is used in search and searchc to commit to the plan computed so far
trans(wait,H,[],[wait|H])    :- !. % wait is a no-op but encourages rolling db
trans(commit,S,[],[commit|S]).	   % commit to the plan found so far! 
trans(abort,S,[],[abort|S]).	   % completely abort execution


% Time bounded steps
% time(E,Sec)  : make the *first* step in E before Sec seconds
% ttime(E,Sec) : make every step before in E before Sec seconds
trans(time(E,Sec),H,E2,H2) :- timeout(trans(E,H,E2,H2), Sec, fail).
final(time(E,Sec),H) :-	timeout(final(E,H), Sec, fail).
trans(ttime(E,Sec),H,time(E2,Sec),H2) :- trans(time(E,Sec),H,E2,H2).
final(ttime(E,Sec),H) :- final(time(E,Sec),H).


% Perform a transition on E, aborting though if an exogenous action happens
% meanwhile and Cond holds in H
% requires exog_interruptable/3 from main cycle
final(exogint(E,_Cond),H) :- final(E,H).
trans(exogint(E,Cond),H,exogint(E2,Cond),H2) :- 
	exog_interruptable(trans(E,H,E2,H2), isTrue(Cond,H), Status),
	(Status=ok -> 
		true 
	; 
		report_message('TF', system(3),'Computation of trans/4 aborted due to exog events'),
		E2=E, H2=H
	).


    /* (B) CONGOLOG CONSTRUCTS                                           */
    /*    iconc(E)    : iterative concurrent execution of E              */
    /*    conc(E1,E2) : concurrent (interleaved) execution of E1 and E2  */
    /*    pconc(E1,E2): prioritized conc. execution of E1 and E2 (E1>E2) */
    /*    bpconc(E1,E2,H): used to improve the performance of pconc(_,_) */
    /*                                                                   */
final(iconc(_),_).
final(conc(E1,E2),H)  :- final(E1,H), final(E2,H).
final(pconc(E1,E2),H) :- final(E1,H), final(E2,H).
trans(iconc(E),H,conc(E1,iconc(E)),H1) :- trans(E,H,E1,H1).

trans(conc(E1,E2),H,conc(E,E2),H1) :- trans(E1,H,E,H1).
trans(conc(E1,E2),H,conc(E1,E),H1) :- trans(E2,H,E,H1).
trans(pconc(E1,E2),H,E,H1) :-    % bpconc(E1,E2,H) is for when E1 blocked at H
    trans(E1,H,E3,H1) -> E=pconc(E3,E2) ; trans(bpconc(E1,E2,H),H,E,H1).

% bpconc(E1,E2,H) does not reconsider process E1 as long as the history
% remains being H (at H, E1 is already known to be blocked)
trans(bpconc(E1,E2,H),H,E,H1) :- !,
    trans(E2,H,E3,H1),  % blocked history H
    (H1=H -> E=bpconc(E1,E3,H) ; E=pconc(E1,E3)).
trans(bpconc(E1,E2,_),H,E,H1) :- trans(pconc(E1,E2),H,E,H1).


       /* INTERRUPTS */
trans(interrupt(Trigger,Body),H,E1,H1) :-
    trans(while(interrupts_running,if(Trigger,Body,?(neg(true)))),H,E1,H1).

trans(interrupt(V,Trigger,Body),H,E1,H1) :- 
    trans(while(interrupts_running, 
    		pi(V,if(Trigger,Body,?(neg(true))))),H,E1,H1).  

final(interrupt(Trigger,Body),H) :-
    final(while(interrupts_running,if(Trigger,Body,?(neg(true)))), H).

final(interrupt(V,Trigger,Body),H) :- 
    final(while(interrupts_running, pi(V,if(Trigger,Body,?(neg(true))))),H).


% BUILT-IN exogenous actions that will be mapped to system actions for the cycle
exog_action(debug).		% Show debugging information	
exog_action(halt).		% Terminate program execution by jumping to the empty program
exog_action(abort).		% Abort program execution by jumping to ?(false) program
exog_action(break).		% Pause the execution of the program
exog_action(reset).		% Reset agent execution from scratch
exog_action(start).		% Start the execution of the program

exog_action(debug_exec).	% Show debugging information	
exog_action(halt_exec).		% Terminate program execution by jumping to the empty program
exog_action(abort_exec).	% Abort program execution by jumping to ?(false) program
exog_action(break_exec).	% Pause the execution of the program
exog_action(reset_exec).	% Reset agent execution from scratch
exog_action(start_exec).	% Start the execution of the program

trans(prioritized_interrupts(L),H,E1,H1) :- 
    expand_interrupts([interrupt(haveExecuted(halt),  halt_exec),
    		       interrupt(haveExecuted(abort), abort_exec),
    		       interrupt(haveExecuted(pause), break_exec),
    		       interrupt(haveExecuted(reset), reset_exec),
		       interrupt(haveExecuted(debug), debug_exec)|L],E), !,
    trans(E,H,E1,H1).

trans(prioritized_interrupts_simple(L),H,E1,H1) :- 
%    expand_interrupts([interrupt(haveExecuted(halt),halt_exec)|L],E), !,
    expand_interrupts(L,E), !,
    trans(E,H,E1,H1).

expand_interrupts([],stop_interrupts).

expand_interrupts([X|L],pconc(X,E)) :-
    expand_interrupts(L,E).

% trans and final for system actions (e.g., show_debug, halt_exec, etc.)    
trans(stop_interrupts,H,[],[stop_interrupts|H]).
final(stop_interrupts,_) :- fail, !.


    /* (C) GOLOG CONSTRUCTS                                           */
    /*                                                                */
    /*  These include primitive action, test, while, pick, if         */
    /*  nondeterministic choice and nondeterministic iteration.	      */
    /*								      */
final([],_).
final(star(_),_).
final(star(_,_),_).
final([E|L],H)       :- final(E,H), final(L,H).
final(ndet(E1,E2),H) :- final(E1,H) ; final(E2,H).
final(if(P,E1,E2),H) :- ground(P), !, (isTrue(P,H) -> final(E1,H) ; final(E2,H)).
final(if(P,E1,_),H)  :- isTrue(P,H), final(E1,H).
final(if(P,_,E2),H)  :- isTrue(neg(P),H), final(E2,H).
final(while(P,E),H)  :- isTrue(neg(P),H) ; final(E,H).

final(pi([],E),H)    :- !, final(E,H).
final(pi([V|L],E),H) :- !, final(pi(L,pi(V,E)),H).
final(pi(V,E),H)     :- !, subv(V,_,E,E2), !, final(E2,H).
final(pi((V,D),E),H) :- !, final(pi(V,D,E),H).
final(pi(V,D,E),H)   :- domain(W,D), subv(V,W,E,E2), !, final(E2,H).

final(rpi([],_,E),H)  :- !, final(E,H).
final(rpi([V|L],E),H) :- !, final(rpi(L,rpi(V,E)),H).
final(rpi((V,D),E),H) :- !, final(rpi(V,D,E),H).
final(rpi(V,D,E),H)   :- rdomain(W,D), subv(V,W,E,E2), !, final(E2,H).

final(E,H)           :- proc(E,E2), !, final(E2,H).

trans([E|L],H,E1,H2)      :- \+ L=[], final(E,H), trans(L,H,E1,H2).
trans([E|L],H,[E1|L],H2)  :- trans(E,H,E1,H2).
trans(?(P),H,[],H)        :- isTrue(P,H).
trans(ndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) ; trans(E2,H,E,H1).
trans(if(P,E1,E2),H,E,H1) :- ground(P), !,
	(isTrue(P,H) -> trans(E1,H,E,H1) ;  trans(E2,H,E,H1)).
trans(if(P,E1,E2),H,E,H1)  :- !,
	((isTrue(P,H), trans(E1,H,E,H1)) ; (isTrue(neg(P),H), trans(E2,H,E,H1))).
trans(star(E,1),H,E1,H1)  :- !, trans(E,H,E1,H1).
trans(star(E,N),H,[E1,star(E,M)],H1)   :- N>1, trans(E,H,E1,H1), M is N-1.
trans(star(E),H,[E1,star(E)],H1)       :- trans(E,H,E1,H1).
trans(while(P,E),H,[E1,while(P,E)],H1) :- isTrue(P,H), trans(E,H,E1,H1).

trans(pi([],E),H,E1,H1)    :- !, trans(E,H,E1,H1).
trans(pi([V|L],E),H,E1,H1) :- !, trans(pi(L,pi(V,E)),H,E1,H1).
trans(pi((V,D),E),H,E1,H1) :- !, trans(pi(V,D,E),H,E1,H1).
trans(pi(r(V),D,E),H,E1,H1):- !, rdomain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).
trans(pi(V,D,E),H,E1,H1)   :- !, domain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).
trans(pi(V,E),H,E1,H1)     :- subv(V,_,E,E2), !, trans(E2,H,E1,H1).
trans(rpi([],E),H,E1,H1)   :- !, trans(E,H,E1,H1).
trans(rpi([V|L],E),H,E1,H1):- !, trans(rpi(L,rpi(V,E)),H,E1,H1).
trans(rpi((V,D),E),H,E1,H1):- !, trans(rpi(V,D,E),H,E1,H1).
trans(rpi(V,D,E),H,E1,H1)  :- rdomain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D) INDIGOLOG SEARCH CONSTRUCTS                               
%%
%% (D.1) search(E,M)  : linear search on E, with message M
%% (D.1) search(E)    : linear search on E	
%% (D.1) searchg(P,E,M) : linear search on E, with message M, replanning condition P
%% (D.1) search(P,E)    : linear search on E, replanning condition P	
%%
%% (D.2) searchc(E,M) : conditional  search on E, with message M   
%% (D.2) searchc(E)   : conditional  search on E
%%
%% (D.3) achieve(G,Max,IA) : CONDITIONAL PLANNER WSCP (Hector Levesque)
%%
%% (D.4) fullSearch   : INTERRUPTABLE SEARCH (BETA VERSION)
%%			still not attached to any Golog construct
%% (D.5) searchr(E,LGoals,FluentAssum,M): RATIONAL SEARCH (BETA VERSION)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.1) TRADITIONAL SEARCH (From [De Giacomo & Levesque 99])
%%
%% Linear plans, ignore sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% search(E): search on E, using caching and replanning only when
%		situation is not the expected one
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
final(search(E,_),H) :- final(search(E),H).
final(search(E),H) :- final(E,H).

trans(search(E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(search(E),H,E1,H1) ->
             report_message(program, 'Finished thinking: Plan found!') 
        ;
             report_message(program, 'Finished thinking: No plan found!'), 
              fail
        ).

% if findpath/3 wants to abort everhting it has to throw exception search
% you can obtain the vanilla search version by having Prolog code to
% ignore both catch/3 and throw/1.
trans(search(E),H,followpath(E1,L),H1) :- 
%	store_node(search, E, H),  % For debugging
        catch( (trans(E,H,E1,H1), findpath(E1, H1, L)) , search, fail).
trans(search(E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(search(E),H,E1,H1) ->
             report_message(program, 'Finished thinking: Plan found!') 
        ;
             report_message(program, 'Finished thinking: No plan found!'), 
             fail
        ).

%
% findpath(E,H,L): find a solution for E at H; 
%		   L is the list [E1,H1,E2,H2,...,EN,HN] encoding
%		   each step evolution (Ei,Hi) where final(EN,HN)
%
% If last action was commit, then try to find a plan with what remains
% if no plan is possible then throw exception "search" to abort the
% whole search
findpath(E,[commit|H],L) :- !, (findpath(E,H,L) -> true ; throw(search)).
findpath(E,H,[E,H]) :- final(E,H).
findpath(E,H,[E,H|L]) :- 
%	store_node(search, E, H),  % For debugging only
        trans(E,H,E1,H1), 
        findpath(E1,H1,L).

% This was the previous version to handle commit by passing around a mark
% meaning failure in the bottom to be propageted up to the top
% This approach would work in vanilla-Prolog without catch/3 but is much
% more demanding computationally, so we use the above version.
%findpath(E,[commit|H],R)    :- findpath(E,H,R).
%findpath(E,[commit|H],fail) :- !.
%findpath(E,H,[E,H]) :- final(E,H).
%findpath(E,H,L)     :- trans(E,H,E1,H1), findpath(E1,H1,L2),
%                       (L2=fail -> (!, L=fail) ; L=[E,H|L2]).


% followpath(E,L): execute program E wrt expected sequence of
%		   configurations L=[E,HEx,E1,H1,...]
%	if the current history does not match the next expected one
% 	in L (i.e., H\=HEx), then redo the search for E from H
final(followpath(E,[E,H]),H) :- !.
final(followpath(E,_),H) :- final(E,H).  /* off path; check again */

trans(followpath(E,[E,H,E1,H1|L]),H,followpath(E1,[E1,H1|L]),H1) :- !.
trans(followpath(E,_),H,E1,H1) :- trans(search(E),H,E1,H1). /* redo search */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% searchg(P,E) : search for E with replanning when P holds only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
final(searchg(_,E,_),H):- final(search(E),H).
final(searchg(_,E),H)  :- final(E,H).

trans(searchg(P,E),H,followpathg(P,E1,L),H1) :- 
        catch( (trans(E,H,E1,H1), findpath(E1,H1,L)), search,fail).
trans(searchg(P,E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(searchg(P,E),H,E1,H1) ->
             report_message(program, 'Finished thinking: Plan found!') 
        ;
             report_message(program, 'Finished thinking: No plan found!'), 
             fail
        ).

% followpath(P,E,L): execute program E wrt expected sequence of
%		     configurations L=[E,H,E1,H1,...]
%	if the current history does not match the next expected one
%	in L and re-planning condition P holds, then redo the search for E at H
final(followpathg(_,E,[E,H]),H) :- !.
final(followpathg(P,E,[E,_]),H) :- \+ isTrue(P,H), !. /* _\=H */
final(followpathg(_,E,_),H) :- final(E,H).  /* off path; check again */

trans(followpathg(P,E,[E,H,E1,H1|L]),H,followpathg(P,E1,[E1,H1|L]),H1) :- !.
trans(followpathg(P,E,_),H,EN,HN) :- 
	isTrue(P,H), !,		/* HExp\= H and replanning cond P holds */
	writeln('we need to replan!'),
	trans(searchg(P,E),H,EN,HN).
trans(followpathg(P,E,[E,HExp,E1,H1|L]),H,followpathg(P,E1,[E1,HN|LN]),HN) :- 
	writeln('NO need to replan!'),
	append(HNExp,HExp,H),	/* HExp\= H and replanning cond P does not holds */
	repair_expected([E1,H1|L],HNExp,[E1,HN|LN]).	/* H=HNExp+HExp */

% repair_expected(L,H,LN): L is a list of expected configurations
%			   LN is the new list of expected configurations
%			   where H is added at the front of each history in L
repair_expected([],_,[]).
repair_expected([E1,H1|L],HNExp,[E1,H11|LN]) :-
	append(HNExp,H1,H11),
	repair_expected(L,HNExp,LN).
	






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.2) CONDITIONAL SEARCH: Conditional plans with sensing  
%%
%% From [Sardina LPAR-01] and based on sGolog [Lakemeyer 99]
%%
%% After a step, the program looks for special "marks" generated by the
%% program when searched:
%%
%% branch(P): CPP should branch w.r.t. rel fluent P 
%% commit   : no backtracking on the already found partial CPP 
%% sim(A)   : A is a simulated exogenous action, don't add it to the CPP
%% test(P)  : a test ?(P) should be left in the CPP (from ??(P))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
final(searchc(E,_),H):- final(searchc(E),H).
final(searchc(E),H)  :- final(E,H).

trans(searchc(E,M),H,E1,H1):- 
        report_message(program, ['Thinking Conditional plan on:      ', M]),
        (trans(searchc(E),H,E1,H1) ->
             report_message(program, 'Finished thinking: CPP found!') 
         ;
             report_message(program, 'Finished thinking: No CPP found!'), 
              fail
        ).

% if calcCPP/3 wants to abort everything it has to throw exception searchc
trans(searchc(E),S,CPP,S):- 
        catch(calcCPP(E,S,CPP), searchc, fail).

trans(branch(P),S,[],[branch(P)|S]).	/* branching step always succeeds */

calcCPP(E,S,[])      :- final(E,S).
calcCPP([E1|E2],S,C) :- E2\=[], !, calcCPP(E1,S,C1), /* program is a sequence */
                        extendCPP(E2,S,C1,C). 

%calcCPP(branch(P),S,[])            :- isTrue(know(P),S), !. /* no branching */
%calcCPP(branch(P),S,[if(P,[],[])]) :- isTrue(kwhether(P),S), !. /* branching */
calcCPP(branch(P),_,[if(P,[],[])]) :- !. /* branching, do not check */

calcCPP(E,S,C) :- trans(E,S,E1,S1),    /* program is not a sequence */
   (S1=[branch(P)|S] -> calcCPP([branch(P)|E1],S,C) ;     /* branch now wrt P*/
%    S1=[commit|S]    -> (calcCPP(E1,S,C) -> /* commit here */
%	                       true 
%		       ;      
%	                 throw(searchc))  ; /* abort if no plan found for E1 */
    S1=S             -> calcCPP(E1,S1,C) ;                /* normal test     */
    S1=[test(P)|S]   -> (calcCPP(E1,S,C1), C=[?(P)|C1]) ; /* perdurable test */
    S1=[A|S]         -> (calcCPP(E1,S1,C1), C=[A|C1]) ).  /* normal action   */

/* extendCPP(E,S,C,C1) recursively descends the CAT C (first two clauses). */
/* Once a leaf of the CAT is reached (third clauses), "calcCPP" is called  */
/* which then extends this branch accordingly to E 		           */
extendCPP(E,S,[sim(A)|C],[sim(A)|C2]) :- exog_action(A), !,
                                         extendCPP(E,[sim(A)|S],C,C2).
extendCPP(E,S,[if(P,C1,C2)],[if(P,C3,C4)]) :- !,  
                assume(P,true,S,S1),  extendCPP(E,S1,C1,C3), 
                assume(P,false,S,S2), extendCPP(E,S2,C2,C4).
extendCPP(E,S,[commit|C],C2) :- !, (extendCPP(E,S,C,C2) ; throw(searchc)).
extendCPP(E,S,[A|C],[A|C2]) :- prim_action(A), !, extendCPP(E,[A|S],C,C2).
extendCPP(E,S,[],C) :- calcCPP(E,S,C).	/* We are on a leaf of the CPP */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.3) CONDITIONAL PLANNER WSCP: conditional planner (Hector Levesque)
%%
%% Requires loading the library for WSCP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(wscplan).  % Load the WSCP Planner

% Transitions for the case(A,CondPlan) construct
trans(case(A,[if(V,Plan)|_]),H,Plan,H):- sensed(A,V,H), !.
trans(case(A,[if(_,_)|BL]),H,case(A,BL),H).

% Transition for the planning construct achieve
% G     : the goal to achieve
% Max   : the maximum depth for the search
% IA    : the initial set of legal actions (list)
% SimNo : number of the exogenous action simulator to use
trans(achieve(G,Max,IA),H,E,H) :- trans(achieve(G,G,Max,IA,none),H,E,H).
trans(achieve(Name,G,Max,IA,SimNo),H,E,H):- wscp(Name,G,Max,IA,SimNo,H,E).
trans(achieve(Name,G,Max,IA,SimNo,Mess),H,E,H):- 
        report_message(program,  ['Planning for: ', Mess]),
        (trans(achieve(Name,G,Max,IA,SimNo),H,E,H) ->
             report_message(program, 'Finished planning: Plan found!') ;
             (report_message(program,'Finished planning: No plan found!'), 
              fail) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.4) INTERRUPTABLE SEARCH (BETA VERSION)
%%
%% Developed first by Hector Levesque (2003)
%% Fixed and improved by Sebastian Sardina (2003-2004)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  This is code to incrementally go through a search tree 
%%
%% Tree is a list [node_k children_k-1 node_k-1 ... children_1 node_1] 
%% where a node is a pair (E,H), E is a program and H is a history     
%% and where children_k is a list of remaining children of node_k      
%% and where node_1 is the root of the tree */
%%
%% Two predicates defined:                                             
%%   - doneSearch(Tree) succeeds iff the search has found a full path  
%%   - xtndSearch(Tree,Tree1) succeeds iff the search from Tree can    
%%     progress one step to Tree1: either extend a leaf of Tree using  
%%     Trans or pop the tree (as necessary) if the leaf goes nowhere   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fullSearch(T, T)  :- doneSearch(T).
fullSearch(T, T1) :- xtndSearch(T, T2), !, fullSearch(T2, T1).

doneSearch([(E,H)|_]) :- final(E,H).

% Progress the tree T one step (poping is not considered a step)
xtndSearch(T,[First,Rest|T]) :-
        T=[(E,H)|_],
%	store_node(xtnd, E, H),  % For debugging
        findall((E1,H1),trans(E,H,E1,H1),[First|Rest]).
%       setof((E1,H1),trans(E,H,E1,H1),[First|Rest]).
xtndSearch([_,[],_,[]|T], T1)          :- !, xtndSearch([backup,[]|T], T1).
xtndSearch([_,[],_,[First|Rest]|T],T1) :- !, xtndSearch([First, Rest|T], T1).
xtndSearch([_,[First|Rest]|T],T1)      :- !, xtndSearch([First, Rest|T], T1).

% Progress the tree T counting each pop as a step
xtndSearch2(T,[First,Rest|T]) :-
        T=[(E,H)|_], setof((E1,H1),trans(E,H,E1,H1),[First|Rest]).
xtndSearch2([_,[],_,[]|T], [_,[]|T]):- !.
xtndSearch2([_,[],_,[First|Rest]|T],[First, Rest|T]) :- !.
xtndSearch2([_,[First|Rest]|T],[First, Rest|T]).

xtnd(T, T, 0) :- !.
xtnd(T, T1, N):- xtndSearch2(T, TT),!, N2 is N-1, xtnd(TT, T1, N2).

/* With these two predicates, we can get a full search without     */
/* any interruptions if desired by calling xtndSearch repeatedly   */
findpath(E,H,Path,H2) :- 
%	store_node(xtnd, E, H),  % For debugging
        fullSearch([(E,H)], T), !,
        buildPath(T, PathR), 
        PathR=[(_,H2)|_],
        reverse(PathR, Path).

buildPath([C],[C]).
buildPath([C,_|R], [C|RP]) :- buildPath(R, RP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.5) RATIONAL SEARCH (BETA VERSION)
%%
%% From [Sardina & Shapiro AAMAS-2003]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Computes the "best" CPP for a program wrt a set of goals
%%
%% The rational search construct needs
%%
%% - E: a nondeterministic ConGolog program
%% - SetGoals: set of pairs goals-rewards: [[G1,R1],...,[Gn,Rn]]
%% - FluentAssum: set of possible assumptions [...,[fi,[v1,..,v2],...]
%%                this set identifies the set of possible worlds for
%%                which all solutions will be tested/evaluated
%% - M: a message for the user when the search starts (optional)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Version with a message
trans(searchr(E,SetGoals,FluentAssum,M),S,CPP,S):- 
        report_message(program, ['Thinking a rational plan on:      ', M]),
        (trans(searchr(E,SetGoals,FluentAssum),S,CPP,S) ->
             report_message(program, 'Finished thinking: rational CPP found!') 
        ;
	     report_message(program, 'Finished thinking: No rat. CPP found!'), 
             fail
        ).

:- dynamic bestPlan/2.

% trans/5 for rational search
trans(searchr(E,SetGoals,FluentAssum),S,CPP,S):- 
	compute_possible_assumptions(FluentAssum, S, SetAssum),
	retractall(bestPlan(_,_)),
        catch(calcRationalCPP(E,S,SetGoals,SetAssum), searchr, bestPlan(CPP)),
	(\+ var(CPP) ; bestPlan(CPP)).

% Compute all CPPs, evaluate them, and store them in the database
% CPP is computed in situation S in which there may be unknowns. 
%     E itself has to be designed to deal with this unknowns (E JIT in S)
calcRationalCPP(E,S,SetGoals,SetAssum) :-
	calcCPP(E,S,CPP),
	calc_vector_utility(CPP, S, SetAssum, SetGoals, LVectorU),
	assert(bestPlan(CPP,LVectorU)),
	fail.
calcRationalCPP(_,_,_,_).

bestPlan(CPP) :-
	bestPlan(CPP,EvalCPP),
	\+ (bestPlan(CPP2, EvalCPP2), 
	    CPP2 \= CPP,
	    member([N,[Min1,_]], EvalCPP),
	    member([N,[_,Max2]], EvalCPP2),
	    Min1<Max2
           ).

%trans(searchr(mainControl(0), [[safeOpen=true,10],[neg(exploded),5]],[[combNum0,[true,false]]]),[],E,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FOR EVALUATING PLANS WRT A SET OF GOALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% calc_vector_utility(+CPP, +H, +SetAssumptions, +SetGoal, -LVectorU) 
%    Computes the vector of utilities of a CPP at H wrt the SetGoals
% LVectorU = [ [MI1,MA1],[MI2,MA2],....,[MIN,MAN] ]
%     where MIk = minimum utility of CPP with assumptions k
%     where MAk = maximum utility of CPP with assumptions k
% SetAssumptions is a list of pairs [Id, History]
calc_vector_utility(CPP, H, SetAssumptions, SetGoals, LVectorU) :-
	findall([Id,U],(member([Id,H2],SetAssumptions),
                        append(H2,H,H3),
			evaluate_CPP(CPP,H3,SetGoals,U)), LU),  
	maplist(get_min_max,LU,LVectorU).

% Obtains the pair minimum-maximum of a list L of numbers
get_min_max([Id,L],[Id,[Min,Max]]) :- min(L, Min), max(L, Max).
	
% compute_possible_assumptions(+FluentAssum, +H, -SetAssumptions)
%  FluentAssum is a list of pairs [Fluent, [v1,v2,...,vn]] where
%        v1,..,vn are the possible assumptions for Fluent
%  H is the current history
%  SetAssumptions are all the possible assumptions that are consistent at H
compute_possible_assumptions(FluentAssum, H, SetAssumptions) :-
	findall(HA, multiple_assumptions(FluentAssum, H, HA), LHA),
	identify_list(LHA, 1, SetAssumptions).

% identify_list(L,N,IL) 
%   If L = [e1,e2,e3,...,en], then IL=[ [N,e1], [N+1,e2], ..., [N+n,en] ]
identify_list([], _, []).
identify_list([A|RA], N, [[N,A]|IRA]) :-
	N2 is N+1,
	identify_list(RA, N2, IRA).

% multiple_assumptions(+LAssum, +H, H2) :
%         H2 is a consistent combination of assumptions at H from LAssum
multiple_assumptions([], _, []).
multiple_assumptions([[F,PV]|R], H, HA) :- % F has no value on H
	holds(neg(know(F)), H), !,
	multiple_assumptions(R, H, HR),
	member(X, PV),
	assume(F,X,HR,HA).
multiple_assumptions([[_,_]|R], H, HA) :- % F has a value in H, do not assume
	multiple_assumptions(R, H, HA).


% Evaluate a CPP in a particular history H wrt goals-value SG
% LUtilities is a list of utilities that the CPP may collect
evaluate_CPP(CPP, H, SG, LUtilities) :-
	findall(U,  (extract_trace(CPP, H, H2),
	             append(H2, H, H3), 
		     evaluate_trace(H3, SG, U)), LUtilities).


% Evaluate history H wrt the set of pairs Goal-Value
evaluate_trace(_, [], 0) :- !.
evaluate_trace(H, [[Goal,Value]|RG], Utility) :-
	evaluate_trace(H,RG,UtilityRG),
	(isTrue(Goal,H) -> 
		Utility is UtilityRG + Value 
	; 
	        Utility is UtilityRG
	).
	

% extract_trace(E,H,H2) : H2 is the execution trace of E starting in H
%                         (E is known to be executable at H)
extract_trace([],_,[]).
extract_trace([if(P,E1,E2)],H,H2) :-
	isTrue(P=Y,H), !,
	(Y=true -> extract_trace(E1,[A|H], H2) ; extract_trace(E2,[A|H], H2)).
extract_trace([if(P,E1,_)],H,HR) :-
	assume(P,true,H,H2),
	extract_trace(E1,H2, H3),
	assume(P,true,[],H4),
	append(H3,H4,HR).
extract_trace([if(P,_,E2)],H,HR) :- !,
	assume(P,false,H,H2),
	extract_trace(E2,H2, H3),
	assume(P,false,[],H4),
	append(H3,H4,HR).
extract_trace([A|E],H,H2) :-   % It is an action followed by a CPP
	A\=[],
	extract_trace(E,[A|H], HE),
	append(HE,[A],H2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (E) SYNCHRONIZATION CONSTRUCT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Syncronize a set of configuration pairs
%%
%% trans(sync(EL),H,sync(EL2),H2):
%%      programs EL can all perform a syncronized step to EL2 and H2
%% final(sync(EL),H):
%%      programs EL can all terminate at H
%%
%% synctrans/4 and syncfinal/2 are a bit more powerful as they may
%%  use different situations for the different programs:
%%
%%    synctrans([E1,E2,...],[H1,H2,...],[E11,E22,...],[H11,H22,...],A)
%%  configurations (Ei,Hi) can advance to (Eii,Hii) by doing action A
%%    syncfinal([E1,E2,...],[H1,H2,...])
%%  configurations (Ei,Hi) can all terminate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans(sync(LE),H,sync(EL2),[A|H]) :-
	length(LE,LL),
	buildListRepeat(LL,H,LH),
	synctrans(LE,LH,EL2,_,A).

final(sync(LE),H) :- 
	length(LE,LL),
	buildListRepeat(LL,H,LH),
	syncfinal(LE,LH).

% buildListRepeat(N,E,L) :- L is a list of N repetitions of element E
buildListRepeat(0,_,[])    :- !.
buildListRepeat(N,H,[H|L]) :- N2 is N-1, buildListRepeat(N2,H,L).

synctrans([E],[H],[E2],[A|H],A)   :- !, ttrans(E,H,E2,[A|H]).
synctrans([E|LP],[H|LH],[E2|LP2],[[A|H]|LH2],A) :- 
	ttrans(E,H,E2,[A|H]), 
	synctrans(LP,LH,LP2,LH2,A).

syncfinal([E],[H])       :- !, tfinal(E,H).
syncfinal([E|LP],[H|LH]) :- tfinal(E,H), syncfinal(LP,LH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LAST TRANS FOR PROCEDURES AND PRIMITIVE ACTIONS (everything else failed)
% Replace the arguments by their value, check that it is a primitive action
% and finally check for preconditions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans(E,H,E1,H1)    :- proc(E,E2), !, trans(E2,H,E1,H1).
trans(A,H,[],[A|H]) :- system_action(A), !.
final(A,_) 	    :- system_action(A), !, fail.


trans(E,H,[],[E1|H])    :- 
	calc_arg(E,E1,H),
	prim_action(E1), 
	poss(E1,P), 
	isTrue(P,H).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% isTrue(P,H) : interface with the projector used. Is P true at H?
%%
%% Currently hooked to eval/3, which should be provided by the projector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SPECIAL PROJECTOR CASES FOR SYSTEM-WIDE FLUENTS
isTrue(interrupts_running,H)      :- !, \+ (H=[stop_interrupts|_]).
isTrue(neg(interrupts_running),H) :- !, \+ isTrue(interrupts_running,H).
%isTrue(last(A),S) 	:- !, S=[A|_]. % true if the last executed action was A
isTrue(haveExecuted(A),S) 	:- !, member(A,S). % true if the A has been executed
isTrue(neg(haveExecuted(A)),S)	:- !, \+ isTrue(haveExecuted(A),S).

% GENERAL PROJECTOR
isTrue(C,H):- eval(C,H,true).

%isTrue(C,H):- eval(C,H,B),          % Base case, use the temporal projector
%	      (B=true    -> true ;
%	       B=false   -> fail ; 
%              B=unknown -> unknown(C,H)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTHER TOOLS
%%
%% do(E,H,H3) : Golog and ConGolog Do/3 macro 
%% ttrans/4: transitive clousure of trans/4
%% ttransn/5: n steps of trans/4
%% tfinal/2: transitive clousure of trans/4 and final/2 combined
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Added for KR course....
do(E,H,H3) :- 
	trans(search(E),H,E2,H2), 
	ttrans(E2,H2,E3,H3),
	final(E3,H3).
	
% Transitive clousure of trans/4
ttrans(E,H,E,H).
ttrans(E,H,E1,H1) :- 
	trans(E,H,E2,H2), 
	(var(H1) -> 
		true 			% always succ if var(H1) 
	; 
		once(before(H2, H1))	% If H1 is given, H2 is a subhistory of H1
	), 				% Avoid infinite ttrans steps
        ttrans(E2,H2,E1,H1).

% transitive version of trans/4 and final/2 combined
tfinal(E,H) :- final(E,H).
tfinal(E,H) :- ttrans(E,H,E2,H), E2\=E, tfinal(E2,H).

% transn/5 performs a defined number N of consequitives trans steps
transn(E,H,E,H,0) :- !.
transn(E,H,E1,H1,N) :- 
	N2 is N-1,
	transn(E,H,E2,H2,N2),
	trans(E2,H2,E1,H1).


% Stores a node/4 entry in DB with Id and program E and history H
store_node(Id, E, H) :-
	(retract(counter(N)) -> N2 is N+1 ; N2=1),
	assert(node(Id,N2,E,H)),
	assert(counter(N2)).

% Evolve (E,H) as much as possible until (E2,H2)
tttrans(E,H,E2,H2) :- ttrans(E,H,E2,H2), \+ trans(E2,H2,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/transfinal.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%