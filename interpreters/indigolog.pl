/* 	INDIGOLOG Interpreter

	This is the main file for the INDIGOLOG interpreter, responsible for
	starting up all devices of an application domain and run teh INDIGOLOG
	program in those devices.

	@contributors 2001-2024
		Sebastian Sardina - ssardina@cs.toronto.edu
		Hector Levesque
		Giuseppe De Giacomo
		Yves Lesperance
		Maurice Pagnucco
		Stavros Vassos

    Part of the INDIGOLOG system
    Refer to root directory for license, documentation, and inforamtion.




   	The main tool provided in this file is the following predicate:

	-- indigolog(E): run INDIGOLOG program E in the framework. It will
	initialize all devices and run the program in those devices. This file
	is consulted by the main application file, e.g., main.pl

	Example run:

	$ swipl config.pl examples/elevator_sim/main.pl
	SYSTEM(0): Debug level set to 5
	SYSTEM(0): Debug level for module em set to 1
	INFO(0): Set wait-at-action enable to: 1 seconds.
	Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.5)
	SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
	Please run ?- license. for legal details.

	For online help and background, visit https://www.swi-prolog.org
	For built-in help, use ?- help(Topic). or ?- apropos(Word).

	?- indigolog(control(congolog_smart)).

	INIT: Starting ENVIRONMENT MANAGER...
	EM(1): Openinig EM server socket...
	INFO(5,APP): Command to initialize device simulator: xterm -e [-e,swipl,-t,start,/home/ssardina/PROJECTS/sitcalc/INDIGOLOG/indigolog.git/env/dev_sim.pl,--host,localhost,--port,8000]
	EM(1): Device simulator initialized at ip(127,0,0,1)
	INIT: ENVIRONMENT MANAGER was started successfully.
	INIT: Starting PROJECTOR EVALUATOR...
	INIT: PROJECTOR was started successfully.
	INFO(5): History updated to: []
	INIT: Starting to execute main program
	INFO(2): Sending action for execution: unset(new_request)

	To set-up options:

	?- set_option.
	set_option(Option, V): sets Option to value V, where Options may be:

	log_level: up to what level to report
	wait_step : pause V seconds after each prim. action execution.
	true.

	?- set_option(log_level, em(3)).
	SYSTEM(0): Debug level for module em set to 3
	true.

	?- set_option(wait_step, 5).
	INFO(0): Set wait-at-action enable to: 5 seconds.
	true ;
*/
:- dynamic sensing/2,   % There may be no sensing action
	pending/1, 			% Stores exogenous events or sensings not yet managed
	now/1,            	% Used to store the actual history
	progressed_history/1, 	% Part of now/1 that was already rolled fwd
	wait_at_action/1, 	% Wait some seconds after each action
	doing_step/0, 		% A step is being calculated
	protect_history/1, 	% Protect a history to avoid rolling forward
	pause_step/0.     	% Pause the step being calculated


% Predicates that they have definitions here but they can defined elsewhere
:- 	multifile(set_option/1),
   	multifile(set_option/2),
   	multifile(initialize/1),
   	multifile(finalize/1),
   	multifile(exog_action/1),
   	multifile(system_action/1),
	% Holds/2 is defined mostly by projector but also in transfinal
	%  	to efficiently support certain constructs (e.g., prioritized-concurrency)
	multifile(holds/2).


:- ensure_loaded(env_man). 	 	% Load environment maanger
:- ensure_loaded(transfinal).	% Load the TRANS and FINAL definitions



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    CONFIGURATION SECTION
%
% This tools allow the user to tune different global options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
set_option(wait_step, N) :- wait_step(N), !.

wait_step(0) :-
	logging(info(0), "** Wait-at-action disabled"),
	retractall(wait_at_action(_)).
wait_step(S) :-
	number(S),
	logging(info(0), "Set wait-at-action enable to: ~d seconds.", [S]),
	retractall(wait_at_action(_)),
	assert(wait_at_action(S)).
wait_step(_) :-
	logging(warning, "Wait-at-action cannot be set!").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    SOME SYSTEM BUILT-IN EXOGENOUS ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BUILT-IN SYSTEM exogenous actions
% These are special actions that if they are in the current history
% they are interpreted by the interpreter in a particular way
% This should be seen as meta-actions that deal with the interpreter itself
system_action(debug_indi).	% printout debug info
system_action(halt_indi).	% force terminateion top level
system_action(end_indi).	% force clean termination
system_action(break_indi).	% break the agent execution to top-level Prolog
system_action(wait_indi(N)) :- ground(N), !.	% Change waiting at action step
system_action(wait_indi(0)).
system_action(wait_exog).

% sys actions can come as exogenous actions
exog_action(A) :- system_action(A).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INITIALIZATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initialize(indigolog) :-
%	set_option(debug_level, 3),
	logging(init, "Starting ENVIRONMENT MANAGER..."),
	initialize(env_manager),
	logging(init, "ENVIRONMENT MANAGER was started successfully."),
	logging(init, "Starting PROJECTOR EVALUATOR..."),
	initialize(evaluator),
	logging(init, "PROJECTOR was started successfully."),
	reset_indigolog_dbs([]). % Reset DB wrt controller

finalize(indigolog)  :-
	logging(end, "INDIGOLOG is finishing..."),
	logging(end, "Finalizing PROJECTOR..."),
	finalize(evaluator),
	logging(end, "PROJECTOR was finalized successfully."),
	logging(end, "Finalizing ENVIRONMENT MANAGER..."),
	finalize(env_manager),
	logging(end, "ENVIRONMENT MANAGER was finalized successfully.").


% Clean all exogenous actions and set the initial now/1 situation
reset_indigolog_dbs(H) :-
	retractall(doing_step),
	retractall(pending(_)),
	retractall(protect_history(_)),
	retractall(progressed_history(_)),
	assert(progressed_history([])),
	update_now(H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    MAIN LOOP
%
% The top level call is indigolog(E), where E is a program
% The history H is a list of actions (prim or exog), initially []
% Sensing reports are inserted as actions of the form e(fluent, value)
%
% indigo/2, indigo2/3, indigo3/3 implement the main architecture by
%      defyining a 3-phase main cycle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% (A) INTERFACE PREDICATE TO THE TOP LEVEL MAIN CYCLE (RUNS ONCE!)
%%
indigolog(E) :-		% Run program E
	(var(E) -> proc(main, E) ; true),
	initialize(indigolog),
	logging(init, "Starting to execute main program"),
	indigolog(E, []), !,
	logging(end, "Execution finished. Closing modules..."),
	finalize(indigolog), !,
	logging(end, "Everything finished - HALTING TOP-LEVEL CONTROLLER").

%%
%% (B) MAIN CYCLE: check exog events, roll forward, make a step.
%%
indigolog(E, H) :-
	% trace,
		% process all pending exog actions, and process sys actions
	(setof(X, retract(pending(exog_action(X))), XL) -> true ; XL = []),
	(	setof(X, (member(X, XL), system_action(X)), HS)
	->	logging(indi(0), "Received sys actions: ~w", [HS])
	; 	HS = []), !,
	(	setof(X, (member(X, XL), \+ system_action(X)), HE)
	->	logging(indi(0), "Received exogenous actions: ~w", [HE])
	; 	HE = []), !,
	append(HE, H, H1),	% append exog actions to history
	process_system_actions(HS, E, H1, E2), !,
		% progress the history (possibly)
	(must_progress(H1) -> progress(H1, H2) ; H2 = H1),
		% make a step in the program (if no exo action ocurrs)
	% trace,
	% OK, now we need to execute (E2, H2)
	catch((assert(doing_step),
			compute_step(E2, H2, E3, H3, T),
			retract(doing_step)),
	 	exog_action, T = exog), !, % done, next cycle
	%  consider all possible options to keep going...
	(	T = trans
	-> 	indigolog(H2, E3, H3)
	;	T = final
	-> 	logging(program,  "Program has executed to completion!! History done:\n\t ~w", [H2])
	;	T = exog
	-> 	logging(program, "Restart INDIGOLOG cycle (exogenous action ocurred!)."),
		indigolog(E3, H3)
	;	T = none
	-> 	logging(program,  "Program fails: \n\t~w\n ...at history:\n\t ~w", [E2, H2]),
		assert(failed_program(E2, H2))
	).

compute_step(E1, H1, _, _, final) :- final(E1, H1).
compute_step(E1, H1, E2, H2, trans) :- trans(E1, H1, E2, H2).
compute_step(_, _, _, _, none).

% process all found system actions HS at configuration (E, H) with EN as new program to keep executing
process_system_actions(HS, E, H, EN) :-
	member(debug_indi, HS),
	logging(info(0), "Request for DEBUGGING"),
	ignore(debug_eval(H)),
	format("~n\tCurrent program:\n\t\t~w~n", [E]),
	format("~n\tCurrent History: \n\t\t~w~n", [H]),
	subtract(HS, [debug_indi], HS2),
	process_system_actions(HS2, E, H, EN).
process_system_actions(HS, E, H, EN) :-
	member(wait_indi(N), HS),
	set_option(wait_step, N),
	subtract(HS, [wait_indi(N)], HS2),
	process_system_actions(HS2, E, H, EN).
process_system_actions(HS, _, _, []) :-
	member(end_indi, HS),
	logging(info(0), "Request for smooth END").
process_system_actions(HS, _, _, []) :-
	member(end_indi, HS),
	logging(info(0), "Request for HALTING"),
	halt.
process_system_actions(HS, E, _, [?(break)|E]) :-
	member(break_indi, HS),
	logging(info(0), "Request for BREAK").
process_system_actions(_, E, _, E).

%%
%% (C) SECOND phase of MAIN CYCLE for transition on the program
%% indigolog(+H1, +E, +H2): called from indigo/2 only after a full Trans on the program
%% 	H1 is the history *before* the transition
%% 	E is the program that remains to execute
%% 	H2 is the history *after* the transition
%%
indigolog(H, E, H) :-
	indigolog(E, H).			% the case of tests (no-action!)
indigolog(H, E, [sim(_)|H]) :- !,
	indigolog(E, H).			% drop simulated actions
indigolog(H, E, [trans(A)|H]) :- !,
	indigolog(E, [A|H]).		% continue under a trans(_) action
indigolog(H, E, [wait_exog|H]) :- !,
	(can_progress(H) -> progress(H, H1) ; H1 = H),
	% wait for an exogenous event to arrive
	% TODO: may be better to wait for a particular one and timeout
	wait_exog_action,
	indigolog(E, H1).
indigolog(H, E, [A|H]) :- % A is a new domain action to be executed
	indi_execute(A, H, H1),
	(	wait_at_action(S)
	->	logging(info(2), "Waiting step for ~d seconds...", [S]),
		sleep(S)
	;	true
	),
	indigolog(E, H1).

% we can use it like ?(wait_exog_action) to wait for an exogenous event
wait_exog_action :-
	logging(info(2), "Waiting for exogenous action to ocurr..."),
	( 	\+ pending(exog_action(_))
	-> 	thread_wait(pending(exog_action(_)), [wait_preds([pending/1])])
	;	true
	).

% Abort mechanism for SWI: throw exception to main thread only
% 	abortStep(swi) is running in the env. manager thread
%		so by the time throw(exog_action) is executed, it could
%		be the case that thread main already retracted doing_step/0
%		from the DB and that mayEvolve/6 is already finished. In that
%		case the event should not be raised
abort_step :- thread_signal(main, (doing_step -> throw(exog_action) ; true)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  EXECUTION OF ACTIONS
%
%  indi_execute(+A, +H, -H2): execute A at H with new history being H2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indi_execute(A, H, H2) :-    % PROCESS OF SENSING ACTIONS
	logging(info(2), "Sending action for execution: ~w", [A]),
	execute_action(A, H, N, SR), !,
	(	SR = failed
	-> logging(action(error), "Action FAILED to execute: ~w", [A, N]),
		H2 = [failed(A)|H]	% Mark failure of action in history
	;	logging(action, "Action EXECUTED: ~w", [[[A, N], sensing(SR)]]),
		handle_sensing(A, [A|H], SR, H2)  % ADD SENSING OUTCOME!
	),
	update_now(H2).
indi_execute(A, _, _) :-
	logging(error, "Action couldn't be executed by interpreter: ~w", [A]),
	halt.

% Updates the current history to H
update_now(H) :-
	retractall(now(_)), assert(now(H)),
	logging(info(5), "History updated to: ~w", [H]).


% progress initial state by cutting down last actions in H1.
% H2 is new (shorter history)
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%