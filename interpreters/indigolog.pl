/* IndiGolog Interpreter

   The main tool provided in this file is the following predicate:

 -- indigolog(E): run IndiGolog program E

           For more information on Golog and some of its variants, see:
               http://www.cs.toronto.edu/~cogrobo/


	@contributors 2001-
		Sebastian Sardina - ssardina@cs.toronto.edu
		Hector Levesque
		Giuseppe De Giacomo
		Yves Lesperance
		Maurice Pagnucco

  This files provides:

 -- indigolog(+E)
       run IndiGolog program E in the main cycle
 -- now(-H)
       H is the current history
 -- pasthist(?H)
       H is a past situation w.r.t. the current one
 -- doingStep
       the main cycle is computing a step
 -- exog_action_occurred(LExoAction)
	to report a list of exog. actions LExogAction to the top-level

 -- exists_pending_exog
       there are exogenous events pending to be dealt
 -- set_option(+O, +V)
       set option O to value V. Current options are:

	+ wait_step 	number of seconds to wait between steps
	+ debug_level 	level for debug messages
	+ type_manager 	define the type of the environment manager (thread/signal)

 -- error(+M)
       an error has occurred with message M
 -- warn(+M)
       warn the user of event M



  The following should be provided for this file:

 LANGUAGE CONSTRUCTS IMPLEMENTATION (transition system):

 -- trans(+P, +H, -P2, -H2)
       configuration (P, H) can perform a single step to configuration (P2, H2)
 -- final(+P, +H)
       configuration (P, H) is terminating

 FROM ENVIRONMENT MANAGER (eng_man.pl):

 -- execute_action(+A, +H, +T, -Id, -S)
	execute action A of type T at history H and resturn sens.
       	S is the sensing outcome, or "failed" if the execution failed
		Id is the identification for the action from the EM
 -- exog_occurs(-L)
	return a list L of exog. actions that have occurred (sync)
 -- initializeEM/0
	environment initialization
 -- finalizeEM/0
	environment finalization
 -- set_type_manager(+T)
       set the implementation type of the env manager


 FROM TEMPORAL PROJECTOR (evalxxx.pl):

 -- debug(+A, +H, -S)
       debug routine
 -- pause_or_roll(+H1, -H2)
       check if the DB CAN roll forward
 -- can_progress(+H1)
       check if the DB CAN roll forward
 -- must_progress(+H1)
       check if the DB MUST roll forward
 -- roll_DB(+H1)
       check if the DB MUST roll forward
 -- initializeDB/0
       initialize projector
 -- finalizeDB/0
       finalize projector
 -- handle_sensing(+A, +H, +Sr, -H2)
	change history H to H2 when action A is executed in history
	H with Sr as returning sensing value
 -- sensing(+A, -SL)	    :
       action A is a sensing action with possible sensing outcome list SL
 -- system_action(+A)      :
       action A is an action used by the system
       e.g., the projector may use action e(_, _) to store sensing outcomes

 FROM THE SPECIFIC DOMAIN OR APPLICATION:

 -- simulate_sensing(+A)
       sensing outcome for action A is simulated
 -- type_prolog(+P)
       name of prolog being used (ecl, swi, vanilla, etc)

 OTHERS TOOLS (PROLOG OR LIBRARIES):

 -- sleep(Sec)             : wait for Sec seconds
 -- turn_on_gc             : turns on the automatic garbage collector
 -- turn_off_gc            : turns off the automatic garbage collector
 -- garbage_collect        : perform garbage collection (now)
 -- logging(+T, +M) : report message M of type T
 -- set_debug_level(+N)    : set debug level to N
*/
:- dynamic sensing/2,   % There may be no sensing action
	pending_event/1, 	% Stores exogenous events or sensings not yet managed
	now/1,            	% Used to store the actual history
	progressed_history/1, 	% Part of now/1 that was already rolled fwd
	wait_at_action/1, 	% Wait some seconds after each action
	doing_step/0, 		% A step is being calculated
	protect_history/1, 	% Protect a history to avoid rolling forward
	pause_step/0.     	% Pause the step being calculated


% Predicates that they have definitions here but they can defined elsewhere
:- multifile(set_option/1),
   multifile(set_option/2),
   multifile(exog_action/1), 	% Many modules can register exog. actions
   multifile(system_action/1).  % Many modules can register system actions



:- ensure_loaded(transfinal).  % Load the TRANS and FINAL definitions



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    CONFIGURATION SECTION
%
% This tools allow the user to tune different global options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set_option/1/2 are used to define parameters the user can set
% set_option/1 is used for the help tool set_option/0
% set_option/2 is the actual definition of the parameter configuration

set_option :-
	writeln("set_option(Option, V): sets Option to value V, where Options may be:"),
	nl,
	set_option(X),
	tab(1),
	writeln(X),
	fail.
set_option.

% Set the wait-at-action to pause after the execution of each prim action
set_option("wait_step : pause V seconds after each prim. action execution.").
set_option(wait_step, N) :- wait_step(N).

wait_step(0) :-
	logging(info(0), "** Wait-at-action disabled"),
	retractall(wait_at_action(_)).
wait_step(S) :-
	number(S),
	logging(info(0), ["** Wait-at-action enable to ", S, " seconds."]),
	retractall(wait_at_action(_)),
	assert(wait_at_action(S)).
wait_step(_) :-
	logging(warning, "Wait-at-action cannot be set!").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    SOME SYSTEM BUILT-IN EXOGENOUS ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BUILT-IN exogenous actions that will be mapped to SYSTEM actions for the cycle

% These are special actions that if they are in the current history
% they are interpreted by the interpreter in a particular way
% This should be seen as meta-actions that deal with the interpreter itself
system_action(debug_indi).	% Special action to force debugging
system_action(halt_indi).	% Action to force clean termination
system_action(abort_indi).	% Action to force sudden nonclean termination
system_action(start_indi).	% Action to start execution
system_action(break_indi).	% Action to break the agent execution to top-level Prolog
system_action(reset_indi).	% Reset agent execution from scratch

exog_action(A) :- system_action(A).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INITIALIZATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init :-
%	set_option(debug_level, 3),
	logging(info(0), "Starting ENVIRONMENT MANAGER..."),
	initialize(env_manager),    	  	% Initialization of environment
	logging(info(0), "ENVIRONMENT MANAGER was started successfully."),
	logging(info(0), "Starting PROJECTOR EVALUATOR..."),
	initialize(evaluator),             	% Initialization of projector
	logging(info(0), "PROJECTOR was started successfully."),
	reset_indigolog_dbs([]).      	% Reset the DB wrt the controller

fin  :-
	logging(info(0), "Finalizing PROJECTOR..."),
	finalize(evaluator),               	% Finalization of projector
	logging(info(0), "PROJECTOR was finalized successfully."),
	logging(info(0), "Finalizing ENVIRONMENT MANAGER..."),
	finalize(env_manager),      		% Finalization of environment
	logging(info(0), "ENVIRONMENT MANAGER was finalized successfully.").


% Clean all exogenous actions and set the initial now/1 situation
reset_indigolog_dbs(H) :-
	retractall(doing_step),
	retractall(pending(_)),
	retractall(protect_history(_)),
	retractall(progressed_history(_)),
	retractall(now(_)),
	update_now(H),
	assert(progressed_history([])),
	fail.
reset_indigolog_dbs(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    MAIN LOOP
%
% The top level call is indigolog(E), where E is a program
% The history H is a list of actions (prim or exog), initially []
% Sensing reports are inserted as actions of the form e(fluent, value)
%
% indigo/2, indigo2/3, indigo3/3 implement the main architecture by
%      defyining a 3-phase main cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% (A) INTERFACE PREDICATE TO THE TOP LEVEL MAIN CYCLE (RUNS ONCE!)
%%
indigolog(E) :-		% Run program E
	(var(E) -> proc(main, E) ; true),
	init,
	logging(info(0), "Starting to execute main program"),
	indigolog(E, []), !,
	logging(info(0), "Execution finished. Closing modules..."),
	fin, !,
	logging(info(0), "Everything finished - HALTING TOP-LEVEL CONTROLLER").

%%
%% (B) MAIN CYCLE: check exog events, roll forward, make a step.
%%
indigolog(E, H) :-
		% process all pending exog actions, sys acitons, and sensing
	findall(E, (pending(exog_action(E)), \+ system_action(E)), HE),
	append(HE, H, H1),
	findall(E, (pending(exog_action(E)), system_action(E)), HS), !,
	process_system_actions(HS, E, H1, E2),
		% progress the history (possibly)
	(must_progress(H1) -> progress(H1, H2) ; H2 = H1),
		% make a step in the program (if no exo action ocurrs)
	catch((assert(doing_step),
			compute_step(E2, H2, E3, H3, T),
			retract(doing_step)),
	 	exog_action, T = exog),
	(T = trans -> indigolog(H2, E3, H3) ;
	 T = final -> logging(program,  "Success final.") ;
	 T = exog -> (logging(program, "Rester step."), indigolog(E3, H3)) ;
	 T = none -> logging(program,  "Program fails.")
	).

compute_step(E1, H1, _, _, final) :- final(E1, H1).
compute_step(E1, H1, E2, H2, trans) :- trans(E1, H1, E2, H2).
compute_step(_, _, _, _, none).


% process all found system actions HS at configuration (E,H)
%	EN is the new program to keep executing
process_system_actions(HS, E, H, EN) :-
	member(debug_indi, HS),
	logging(info(0), "Request for DEBUGGING"),
	ignore(debug(H)),
	format("Curent program:~w\t ~w", [E]),
	delete(debug_indi, HS, HS2),
	process_system_actions(HS2, EN).
process_system_actions(HS, _, _, []) :-
	member(halt_indi, HS),
	logging(info(0), "Request for HALTING").
process_system_actions(HS, E, _, [?(break)|E]) :-
	member(break_indi, HS),
	logging(info(0), "Request for BREAK").


%%
%% (C) SECOND phase of MAIN CYCLE for transition on the program
%% indigolog(+H1, +E, +H2): called from indigo/2 only after a successful Trans on the program
%% 	H1 is the history *before* the transition
%% 	E is the program that remains to execute
%% 	H2 is the history *after* the transition
%%
indigolog(H, E, H) :-
	indigolog(E, H).	% the case of Trans for tests
indigolog(H, E, [sim(_)|H]) :- !,
	indigolog(E, H).	% drop simulated actions
indigolog(H, E, [wait|H]) :- !,
	(can_progress(H) -> progress(H, H1) ; H1 = H),
	logging(info(2), "Waiting for exogenous action to ocurr..."),
	% wait for an exogenous event to arrive
	% TODO: may be better to wait for a particular one and timeout
	(	\+ pending(exog_action(_))
	-> thread_wait(pending(exog_action(_)), [wait_preds([pending_event/1])])
	;	true
	),
	indigolog(E, H1).
indigolog(H, E, [stop_interrupts|H]) :- !,
	indigolog(E, [stop_interrupts|H]).
indigolog(H, E, [A|H]) :-
	indixeq(A, H, H1),
	indigolog(E, H1).  % DOMAIN ACTION





% Abort mechanism for SWI: throw exception to main thread only
% 	abortStep(swi) is running in the env. manager thread
%		so by the time throw(exog_action) is executed, it could
%		be the case that thread main already retracted doing_step/0
%		from the DB and that mayEvolve/6 is already finished. In that
%		case the event should not be raised
abort_step :- thread_signal(main, (doing_step -> throw(exog_action) ; true)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  EXECUTION OF ACTIONS
%
%  indixeq(+Act, +H, -H2) is called when action Act should be executed at
%    history H. H2 is the new history after the execution of Act in H
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indixeq(Act, H, H2) :-    % PROCESS SYSTEM ACTIONS: just add it to history
	system_action(Act), !,
	H2 = [Act|H],
	update_now(H2).
indixeq(Act, H, H2) :-    % PROCESS OF SENSING ACTIONS
	sensing_action(Act), !,
	logging(info(1), "Sending sensing action for execution: ~w", [Act]),
	execute_action(Act, H, sensing, IdAct, SR), !,
	(	SR = failed
	-> logging(error, "Action *~w* FAILED to execute at history.", [Act, IdAct]),
		H2 = [abort, failed(Act)|H],	% Request abortion of program
		update_now(H2)
	;	logging(action, "Action *~w* EXECUTED with outcome: ", [[Act, IdAct], SR]),
		handle_sensing(Act, [Act|H], SR, H2),  % ADD SENSING OUTCOME!
		update_now(H2)
	).
indixeq(Act, H, H2) :-         % EXECUTION OF NON-SENSING ACTIONS
	\+ system_action(Act), \+ sensing_action(Act), !,
	logging(info(1), "Sending action for execution: ~w", [Act]),
	execute_action(Act, H, normal, IdAct, S), !,
	(	S = failed
	->	logging(error, "Action FAILED to execute: ~w", [Act, IdAct]),
		H2 = [abort, failed(Act)|H],
	    update_now(H2)
	;   logging(action, "Action EXECUTED SUCCESFULLY: ~w", [Act, IdAct]),
		H2 = [Act|H],
		update_now(H2)
	).


% Updates the current history to H
update_now(H) :- retractall(now(_)), assert(now(H)).



progress(H1, H2) :-
	logging(info(0), "Rolling down the river (progressing the database)......."),
	progress_db(H1, H2),
	logging(info(0), "done progressing the database!"),
	logging(info(3), "New History: ~w", [H2]),
	update_now(H2), 			% Update the current history
	append(H2, HDropped, H1),	% Extract what was dropped from H1
	retract(progressed_history(HO)),		% Update the progressed_history/1 predicate to store all that has been rolled forward
	append(HDropped, HO, HN),			% progressed_history(H): H is the full system history
	assert(progressed_history(HN)),
	save_exog.	% Collect all exogenous actions


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  OTHER PREDICATES PROVIDED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% H is a past situation w.r.t. the actual situation (stored in clause now/1)
pasthist(H) :- now(ActualH), before(H, ActualH).

% Deal with an unknown configuration (P, H)
error(M) :-
        logging(error, M),
        logging(error, "Execution will be aborted!"), abort.

warn(M) :-
        logging(warning, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%