#!/usr/bin/env swipl
/*      Elevator Simulator Environment
        @author Sebastian Sardina (2003) - ssardina@gmail.com

This files provides a *simulated* environment interface with which it is possible to set exogenous events in an asynchronous ways using a TCL/TK application, type sensing outcome for actions

The interface to enter exogenous events from the keyboard is achieved with a simple TCL/TK program where exogenous action can be typed at any time.


 This environment is self-contained (automatically it loads the required
  libraries). It should be called as follows:

  $ swipl config.pl env/env_sim.pl --host=localhost --port=8000 --debug=2

 The generic interface for environment device managers is in env_gen.pl:

 -- start/0     : initialization of the environment (called when loaded)
 -- finalize/0  : finalization of the environment (called when exiting)
 -- report_exog_event(A, M):
                  report exogenous event A with message M to the
                  environment manager

 -- The following two dynamic predicates should be available:
    -- listen_to(Type, Name, Channel)
            listen to Channel of Type (stream/socket) with Name
    -- terminate/0
            order the termination of the application


 -- The following should be implemented here:

  -- name_dev/1              : mandatory *
  -- initializeInterfaces(L) : mandatory *
  -- finalizeInterfaces(L)   : mandatory *
  -- execute/4               : mandatory *
  -- handle_steam/1          : as needed
  -- listen_to/3             : as needed

 FROM PROLOG DEPENDENT USER LIBRARY (SWI, ECLIPSE, LIBRARY):

 -- call_to_exec(+System, +Command, -Command2)
      Command2 executes Command in plataform System


 Also, this device manager requires:

    -- wish for running TCL/TK applications
    -- exog.tcl TCL/TK script
*/


:- include(env_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER
:- ['../lib/common.pl'] .	% Load system library
:- use_module('../lib/utils.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Name of the environment: <SIMULATOR>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(simulator).

% Set verbose debug level
:- set_option(log_level, 3).

/* A - INITIALIZATION AND FINALIZATION OF INTERFACES

TCL/TK EXOGENOUS ACTIONS  GENERATOR - from keyboard via Tcl/Tk interface

This part implements a keyboard interface to enter exogenous events in an asynchronous manner.

An TCL/TK independent process is initiated to read exogenous events the program exog.tcl writes each exogenous action entered to a special pipe. At that time, a sigio signal is assigned to such pipe so that whenever data arrives to the pipe an interrupt is triggered which can be cached by the main cycle to handle thestart exog action entered.
*/
initialize_interfaces :- initialize_exog(tcltk).
initialize_interfaces :- finalize_exog(tcltk).

initialize_exog(tcltk) :-
        printKbInstructions,
        dir(exog_tcltk_, TclFile),
        % Run the TCLK window as a child and send its *output* to pipe "tcltk"
        process_create(path(wish), [TclFile], [stdout(pipe(OutStream)), process(PID)]),
        set_stream(OutStream, alias(tcltk)),
        sleep(2),    % Give time to TCL/TK program to appear
        assert(exog_proc(PID)),
        assert(listen_to(tcltk, [read(OutStream), pid(PID)])).  % listen to tcltk
finalize_exog(tcltk) :-
	listen_to(tcltk, L), % tcltk is still open
        logging(system(1), 'Closing TCL-TK interface.'),
        close(tcltk),
        member(pid(PID), L),
        process_kill(PID).
finalize_exog(tcltk).	% It was already down

% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    writeln('*********************************************************'),
    writeln('* NOTE: This is the SIMULATOR environment'),
    writeln('*   You can enter exogenous actions using the TCL/TK window.'),
    writeln('*   Action execution will be printed here and sensing '),
    writeln('*   outcome will be asked to the user'),
    writeln('*   Actions that are not executed in any other device are'),
    writeln('*   executed here.'),
    writeln('*********************************************************'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle tcl/tk stream: called when there is data comming from the tcl/tk app
handle_stream(tcltk) :-
        read(tcltk, T),
        (       T = end_of_file
        ->      true          % Tcl/Tk finished
        ;       report_exog_event(A, ['Exogenous action *', A, '* received from TCL/TK'])
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, N, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulate the execution of Action.
% SensingResult is the sensing outcome of Action
execute(Action, T, _, Sensing) :-
	member(T, [sensing, simsenstartsing]), !,
        logging(action, ['Executing sensing action: *',Action,'*']),
        write('    ------------> Enter Sensing value, terminate with ".": '),
        read(Sensing), nl.

execute(Action, _, _, ok) :-
        logging(action, ['Executing non-sensing action: *',Action,'*']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% OTHER CODE %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
my_system_error_handler(E, Goal) :-
        (
            errno_id(`Interrupted system call`),
%            errno_id(170, M), errno_id(M),  % M is "Unknown error 170" ??
            restartable_builtin(Goal)
        ->
            call(Goal)
        ;
            errno_id(M),
            logging(error, M),
            read(_),
            error(default(E), Goal)
        ).

% Builtins that can raise EINTR and can be restarted after that
restartable_builtin(accept(_,_,_)).
restartable_builtin(cd(_)).
restartable_builtin(close(_)).
restartable_builtin(connect(_,_)).
restartable_builtin(select_stream(_,_,_)).
restartable_builtin(stream_select(_,_,_)).
restartable_builtin(wait(_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exogenous action window in SWI itself (instead of TCL/TK)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
:- use_module(library(pce)).

fileviewer(Dir) :-
        new(F, frame('File Viewer')),
        send(F, append(new(B, browser))),
        send(new(D, dialog), below(B)),
        send(D, append(button(view,
                              message(@prolog, view,
                                      B?selection?key)))),
        send(D, append(button(quit,
                              message(F, destroy)))),
        send(B, members(directory(Dir)?files)),
        send(F, open).

view(F) :-
        send(new(V, view(F)), open),
        send(V, load(F)).



%:- pce_autoload(file_item, library(file_item)).


edit_file_dialog :-
        new(D, dialog('Exogenous Events')),
        send(D, append, new(E, text_item(exog, @default,
						and(message(@prolog, reportTea, @arg1),
					    	    message(@receiver,clear))
					))),
        send(D, append, button(send,
				and(message(@prolog, reportTea, E?selection),
				    message(E,clear))
			)),
        send(D, append, button(cancel, message(D, destroy))),
        send(D, append, button(halt,   message(@prolog, terminateTea))),
        send(D, open).


reportTea(E) :- logging(action, ['Executing non-sensing action: *',E,'*']).
terminateTea :- logging(action, terminate).



	%	ask_name(+Prompt, +Label, -Name)
	%	Put a prompter on the screen and wait till the user has
	%	entered a name.  Pressing cancel makes this predicate fail.
	%	Prompt is a long string, giving explanation; Label is a short
	%	label displayed for the text entry field.


	:- pce_global(@name_prompter, make_name_prompter).

	make_name_prompter(P) :-
		new(P, dialog),
		send(P, kind, transient),
		send(P, append, label(prompt)),
		send(P, append,
		        new(TI, text_item(name, '',
				 message(P?ok_member, execute)))),
		send(P, append, button(ok, message(P, return, TI?selection))),
		send(P, append, button(cancel, message(P, return, @nil))).


	ask_name(Prompt, Label, Name) :-
		send(@name_prompter?prompt_member, selection, Prompt),
		send(@name_prompter?name_member, label, Label),
		send(@name_prompter?name_member, clear),
		get(@name_prompter, confirm_centered, RawName),
		send(@name_prompter, show, @off),
		RawName \== @nil,
		Name = RawName.

	ask_name :-
		ask_name('Street', name, Street),
		writeln(Street).


create_fill_pattern_dialog :-
	new(Dialog, dialog('Fill Patterns')),
	send(Dialog, append,
		 new(M, menu(fill_pattern, cycle,
						 message(@prolog, write_ln, @arg1)))),
		send_list(M, append,
			[ menu_item(white,  @default, opcion1)
			, menu_item(grey12, @default, opcion2)
			, menu_item(grey25, @default, opcion3)
			, menu_item(grey50, @default, opcion4)
			]),
	send(Dialog, open).

*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%