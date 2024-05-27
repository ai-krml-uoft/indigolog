% #!/usr/bin/env swipl
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

:- ['../config.pl'].	% Load system library
:- ['../lib/common.pl'].	% Load system library
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
finalize_interfaces :- finalize_exog(tcltk).

initialize_exog(tcltk) :-
        printKbInstructions,
        dir(exog_tcltk_, TclFile),
        % run the TCLK window as a child and send its *output* to pipe "tcltk"
        process_create(path(wish), [TclFile], [stdout(pipe(OutStream)), process(PID)]),
        set_stream(OutStream, alias(tcltk)),
        sleep(2),    % give time to TCL/TK program to appear
        assert(listen_to(tcltk, OutStream, [pid(PID)])),  % listen to tcltk
        add_stream_to_pool(tcltk, handle_stream(tcltk)).
finalize_exog(tcltk) :-
        logging(info(1), 'Closing TCL-TK interface.'),
	delete_stream_from_pool(tcltk),
        listen_to(tcltk, _, L),
        member(pid(PID), L),
        process_kill(PID),
        logging(info(1), 'TCL-TK interface closed: stream and process.').

finalize_exog(tcltk).	% already down

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
% similar to failure-driven loop: https://www.swi-prolog.org/pldoc/doc_for?object=repeat/0
handle_stream(tcltk) :-
        read(tcltk, Term),
        writeln(Term),
        (       Term == end_of_file
        ->      finalize_exog(tcltk)
        ;       report_exog(Term)
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
        logging(action, 'EXECUTE SENSING ACTION: ~w', [Action]),
        write('\t---------> Enter Sensing value, terminate with ".": '),
        read(Sensing), nl.

execute(Action, _, _, ok) :-
        logging(action, 'EXECUTE ACTION: ~w', [Action]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%