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
 -- can_roll(+H1)
       check if the DB CAN roll forward
 -- must_roll(+H1)
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

 -- simulateSensing(+A)
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
	indi_exog/1, 		% Stores exogenous events not managed yet
	now/1,            	% Used to store the actual history
	rolled_history/1, 	% Part of now/1 that was already rolled fwd
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
	logging(system(0), "** Wait-at-action disabled"),
	retractall(wait_at_action(_)).
wait_step(S) :-
	number(S),
	logging(system(0), ["** Wait-at-action enable to ", S, " seconds."]),
	retractall(wait_at_action(_)),
	assert(wait_at_action(S)).
wait_step(_) :-
	logging(warning, "Wait-at-action cannot be set!").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    SOME SYSTEM BUILT-IN EXOGENOUS ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BUILT-IN exogenous actions that will be mapped to system actions for the cycle
exog_action(debug).	% Show debugging information
exog_action(halt).		% Terminate program execution by jumping to the empty program
exog_action(abort).		% Abort program execution by jumping to ?(false) program
exog_action(break).		% Pause the execution of the program
exog_action(reset).		% Reset agent execution from scratch
exog_action(start).		% Start the execution of the program

exog_action(debug_exec).	% Show debugging information
exog_action(halt_exec).		% Terminate program execution by jumping to the empty program
exog_action(abort_exec).		% Abort program execution by jumping to ?(false) program
exog_action(break_exec).	% Pause the execution of the program
exog_action(reset_exec).		% Reset agent execution from scratch
exog_action(start_exec).		% Start the execution of the program

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
init :-
%	set_option(debug_level, 3),
	logging(system(0), "Starting ENVIRONMENT MANAGER..."),
	initialize(env_manager),    	  	% Initialization of environment
	logging(system(0), "ENVIRONMENT MANAGER was started successfully."),
	logging(system(0), "Starting PROJECTOR EVALUATOR..."),
	initialize(evaluator),             	% Initialization of projector
	logging(system(0), "PROJECTOR was started successfully."),
	reset_indigolog_dbs([]).      	% Reset the DB wrt the controller

fin  :-
	logging(system(0), "Finalizing PROJECTOR..."),
	finalize(evaluator),               	% Finalization of projector
	logging(system(0), "PROJECTOR was finalized successfully."),
	logging(system(0), "Finalizing ENVIRONMENT MANAGER..."),
	finalize(env_manager),      		% Finalization of environment
	logging(system(0), "ENVIRONMENT MANAGER was finalized successfully.").


% Clean all exogenous actions and set the initial now/1 situation
reset_indigolog_dbs(H) :-
	retractall(doing_step),
	retractall(indi_exog(_)),
	retractall(protect_history(_)),
	retractall(rolled_history(_)),
	retractall(now(_)),
	update_now(H),
	assert(rolled_history([])),
	assert((indi_exog(_) :- fail)),
	fail.
reset_indigolog_dbs(_).


%%
%% (A) INTERFACE PREDICATE TO THE TOP LEVEL MAIN CYCLE
%%
indigolog(E) :-		% Run program E
	(var(E) -> proc(main, E) ; true),
	init,
	logging(system(0), "Starting to execute main program"),
	indigolog(E, []), !,
	logging(system(0), "Execution finished. Closing modules..."),
	fin, !,
	logging(system(0), "Everything finished - HALTING TOP-LEVEL CONTROLLER").

%%
%% (B) MAIN CYCLE: check exog events, roll forward, make a step.
%%
indigolog(E, H) :-
	handle_rolling(H, H2), !, 		% Must roll forward?
	handle_exog(H2, H3),   !, 		% Handle pending exog. events
	catch((assert(doing_step),
			compute_step(E, H3, E4, H4, T),
			retract(doing_step)),
	 	exog_action, T = exog),
	(T = trans -> indigolog(H3, E4, H4) ;
	 T = final -> logging(program,  "Success final.") ;
	 T = exog  -> (logging(program, "Restarting step."), indigolog(E, H3)) ;
	 T = none-> logging(program,  "Program fails.")
	).

compute_step(E1, H1, E2, H2, T) :-
	(exists_pending_exog_event -> throw(T) ; true),
	(	final(E1, H1)
	->	T = final
	;	trans(E1, H1, E2, H2)
	->	T = trans
	;	T = none
	).



%%
%% (C) SECOND phase of MAIN CYCLE for transition on the program
%% indigolog(+H1, +E, +H2): called from indigo/2 only after a successful Trans on the program
%% 	H1 is the history *before* the transition
%% 	E is the program that remains to execute
%% 	H2 is the history *after* the transition
%%
indigolog(H, E, H)          :-
	indigolog(E, H).	% The case of Trans for tests
indigolog(H, E, [sim(_)|H]) :- !,
	indigolog(E, H).	% Drop simulated actions
indigolog(H, E, [wait|H])   :- !,
	pause_or_roll(H, H1),
	doWaitForExog(H1, H2),
	indigolog(E, H2).
indigolog(_, E, [debug_exec|H]) :- !,
	logging(system(0), "Request for DEBUGGING"),
	debug(debug, H, null),
	delete(H, debug, H2),
	length(H2, LH2),
	assert(debuginfo(E, H2, LH2)), !,
	indigolog(E, H2).
indigolog(_, _, [halt_exec|H]) :- !,
	logging(system(0), "Request for TERMINATION of the program"),
	indigolog([], H).
indigolog(_, _, [abort_exec|H]) :- !,
	logging(system(0), "Request for ABORTION of the program"),
	indigolog([?(false)], H).
indigolog(_, E, [break_exec|H]) :- !,
	logging(system(0), "Request for PAUSE of the program"),
	writeln(E),
	break,		% BREAK POINT (CTRL+D to continue execution)
	delete(H, pause, H2),
	indigolog(E, H2).
indigolog(_, _, [reset_exec|_]) :- !,
	logging(system(0), "Request for RESETING agent execution"),
	finalizeDB,
	initializeDB,
	proc(main, E),		% obtain main agent program
	indigolog(E, []).		% restart main with empty history
indigolog(H, E, [stop_interrupts|H]) :- !,
	indigolog(E, [stop_interrupts|H]).
indigolog(H, E, [A|H]) :-
	indixeq(A, H, H1),
	indigolog(E, H1).  % DOMAIN ACTION

% This are special actions that if they are in the current history
% they are interpreted by the interpreter in a particular way
% This should be seen as meta-actions that deal with the interpreter itself
system_action(debug_exec).	% Special action to force debugging
system_action(halt_exec).	% Action to force clean termination
system_action(abort_exec).	% Action to force sudden nonclean termination
system_action(start_exec).	% Action to start execution
system_action(break_exec).	% Action to break the agent execution to top-level Prolog
system_action(reset_exec).	% Reset agent execution from scratch

% Wait continously until an exogenous action occurrs
doWaitForExog(H1, H2) :-
        logging(system(2), "Waiting for exogenous action to happen"),
        repeat,
        handle_exog(H1, H2),
        (H2 = H1 -> fail ; true).



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

% type_action(Action, Type) : finds out the type of an action
type_action(Act, sensing)    :- sensing(Act, _), !.
type_action(Act, system)     :- system_action(Act), !.
type_action(_, nonsensing).

indixeq(Act, H, H2) :-    % EXECUTION OF SYSTEM ACTIONS: just add it to history
        type_action(Act, system), !,
        H2 = [Act|H],
        update_now(H2).
indixeq(Act, H, H2) :-    % EXECUTION OF SENSING ACTIONS
        type_action(Act, sensing), !,
        logging(system(1), ["Sending sensing Action *", Act, "* for execution"]),
        execute_action(Act, H, sensing, IdAct, S), !,
	(S = failed ->
		logging(error, ["Action *", Act, "* FAILED to execute at history: ", H]),
		H2 = [abort, failed(Act)|H],	% Request abortion of program
	        update_now(H2)
	;
                logging(action,
                	["Action *", (Act, IdAct), "* EXECUTED SUCCESSFULLY with sensing outcome: ", S]),
	        wait_if_neccessary,
		handle_sensing(Act, [Act|H], S, H2),  % ADD SENSING OUTCOME!
		update_now(H2)
	).
indixeq(Act, H, H2) :-         % EXECUTION OF NON-SENSING ACTIONS
        type_action(Act, nonsensing), !,
        logging(system(1), ["Sending nonsensing action *", Act, "* for execution"]),
        execute_action(Act, H, nonsensing, IdAct, S), !,
	(S = failed ->
		logging(error, ["Action *", Act, "* could not be executed at history: ", H]),
		H2 = [abort, failed(Act)|H],
	        update_now(H2)
	;
                logging(action, ["Action *", (Act, IdAct), "* COMPLETED SUCCESSFULLY"]),
		wait_if_neccessary,
                H2 = [Act|H],
		update_now(H2)
	).

% Simulated pause between execution of actions if requested by user
wait_if_neccessary :-
        wait_at_action(Sec), !,   % Wait Sec numbers of seconds
        logging(system(2), ["Waiting at step ", Sec, " seconds"]),
        sleep(Sec).
wait_if_neccessary.

% Updates the current history to H
update_now(H) :-
        %logging(system(2), ["Updating now history to: ", H]),
        %write(H),
        retract(now(_)) -> assert(now(H)) ; assert(now(H)).

action_failed(Action, H) :-
	logging(error, ["Action *", Action, "* could not be executed",
	                      " at history: ", H]),
	halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  EXOGENOUS ACTIONS
%
%  Exogenous actions are stored in the local predicate indi_exog(Act)
%  until they are ready to be incorporated into the history
% History H2 is H1 with all pending exog actions placed at the front
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_exog(H1, H2) :-
	save_exog,				% Collect on-demand exogenous actions
	exists_pending_exog_event,		% Any indi_exog/1 in the database?
		% 1 - Collect SYSTEM exogenous actions (e.g., debug)
	findall(A, (indi_exog(A), type_action(A, system)), LSysExog),
		% 2 - Collect NON-SYSTEM exogenous actions (e.g., domain actions)
	findall(A, (indi_exog(A), \+ type_action(A, system)), LNormal),
		% 3 - Append the lists to the current hitory (system list on front)
	append(LSysExog, LNormal, LTotal),
	append(LTotal, H1, H2),
	update_now(H2),
		% 4 - Remove all indi_exog/1 clauses
	retractall(indi_exog(_)).
handle_exog(H1, H1). 	% No exogenous actions, keep same history


% Collect on-demand exogenous actions: reported  by exog_occurs/1
save_exog :- exog_occurs(L) -> store_exog(L) ; true.

store_exog([]).
store_exog([A|L]) :- assertz(indi_exog(A)), store_exog(L).

% Is there any pending exogenous event?
exists_pending_exog_event :- indi_exog(_).


% exog_action_occurred(L) : called to report the occurrence of a list L of
% 				exogenous actions (called from env. manager)
%
% First we add each exogenous event to the clause indi_exog/1 and
% in the end, if we are performing an evolution step, we abort the step.
exog_action_occurred([]) :- doing_step -> abortStep ; true.
exog_action_occurred([ExoAction|LExoAction]) :-
        assert(indi_exog(ExoAction)),
        logging(exogaction, ["Exog. Action *", ExoAction, "* occurred"]),
	exog_action_occurred(LExoAction).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLING OF ROLLING FORWARD
%
% handle_rolling/2: mandatory rolling forward
% pause_or_roll/2: optional rolling forward
%
% Based on the following tools provided by the evaluator used:
%
%	must_roll(H): we MUST roll at H
%	can_roll(H) : we COULD roll at H (if there is time)
%	roll_db(H1, H2): roll from H1 to H2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_rolling(H1, H2) :- must_roll(H1), !, roll(H1, H2).
handle_rolling(H1, H1).

pause_or_roll(H1, H2) :- can_roll(H1), !, roll(H1, H2).
pause_or_roll(H1, H1).


roll(H1, H2) :-
        logging(system(0), "Rolling down the river (progressing the database)......."),
	roll_db(H1, H2),
        logging(system(0), "done progressing the database!"),
        logging(system(3), ["New History: ", H2]),
	update_now(H2), 			% Update the current history
	append(H2, HDropped, H1),	% Extract what was dropped from H1
	retract(rolled_history(HO)),		% Update the rolled_history/1 predicate to store all that has been rolled forward
	append(HDropped, HO, HN),			% rolled_history(H): H is the full system history
	assert(rolled_history(HN)),
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