%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* A simulator device manager on console

    Part of the INDIGOLOG system

    Refer to root directory for license, documentation, and information

This files provides a *simulated* environment interface with which it is
possible to set exogenous events in an asynchronous ways using a TCL/TK
application, type sensing outcome for actions

The interface to enter exogenous events from the keyboard is achieved
with a simple TCL/TK program where exogenous action can be typed at any
time.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include(dev_gen).      % INCLUDE THE CORE OF THE DEVICE MANAGER

:- ['../config.pl'].	        % Load system library
:- ['../lib/common.pl'].	% Load system library
:- use_module('../lib/utils.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Name of the environment: <SIMULATOR>
% Set name of the environment here.
% THIS CONSTANT IS MANDATORY, DO NOT DELETE!
name_dev(simulator).

% Set verbose debug level
:- set_option(log_level, 3).

/* A - INITIALIZATION AND FINALIZATION OF INTERFACES

TCL/TK EXOGENOUS ACTIONS  GENERATOR - from keyboard via Tcl/Tk interface

This part implements a keyboard interface to enter exogenous events in an
asynchronous manner.

An TCL/TK independent process is initiated to read exogenous events the program
exog.tcl writes each exogenous action entered to a special pipe. At that time, a
sigio signal is assigned to such pipe so that whenever data arrives to the pipe
an interrupt is triggered which can be cached by the main cycle to handle
thestart exog action entered.
*/
initialize_interfaces :- initialize(tcltk).
finalize_interfaces :- finalize(tcltk).

initialize(tcltk) :-
        print_instructions,
        % run the TCLK window as a child and send its *output* to pipe "tcltk"
        % dir(exog_tcltk_, File),
        % process_create(path(wish), [File], [stdout(pipe(OutStream)), process(PID)]),
        % run the TCLK window as a child and send its *output* to pipe "tcltk"
        dir(exog_python_tcltk_, File),
        process_create(path(python), [File, "--events", "end_indi"], [stdout(pipe(OutStream)), process(PID)]),
        set_stream(OutStream, alias(tcltk)),
        sleep(2),    % give time to TCL/TK program to appear
        assert(listen_to(tcltk, OutStream, [pid(PID)])),  % listen to tcltk
        add_stream_to_pool(tcltk, handle_stream(tcltk)).
finalize(tcltk) :-
        logging(info(2), 'Closing TCL-TK interface.'),
	delete_stream_from_pool(tcltk),
        listen_to(tcltk, _, L),
        member(pid(PID), L),
        process_kill(PID),
        logging(info(2), 'TCL-TK interface closed: stream and process.').
finalize(tcltk).	% already down

print_instructions :-
    writeln('*********************************************************'),
    writeln('* NOTE: This is the SIMULATOR environment'),
    writeln('*   You can enter exogenous actions using the TCL/TK window.'),
    writeln('*   Action execution will be printed here and sensing '),
    writeln('*   outcome will be asked to the user'),
    writeln('*   Actions that are not executed in any other device are'),
    writeln('*   executed here.'),
    writeln('*********************************************************'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - HANDLERS FOR EACH STREAM/SOCKET THAT IS BEING HEARD:  handle_stream/1
%
% HERE YOU SHOULD WRITE HOW TO HANDLE DATA COMMING FROM EACH OF THE
% INTERFACES/CHANNELS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle tcl/tk stream: called when there is data comming from the tcl/tk app
% similar to failure-driven loop:
%       https://www.swi-prolog.org/pldoc/doc_for?object=repeat/0
handle_stream(tcltk) :-
        logging(info(4), "Reading term from TCL/TK interface"),
        read_term(tcltk, Term, []),
        (       Term == end_of_file
        ->      finalize(tcltk)
        ;       get_char(tcltk, '\n'), % read last \n that tcl adds at end
                report_exog(Term)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - EXECUTION MODULE: execute/4
%
% This part implements the execution capabilities of the environment
%
% execute(Action, Type, N, Sensing) : execute Action of Type and return Sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulate the execution of Action.
% SensingResult is the sensing outcome of Action
execute(sense(A), _, Sensing) :- !,
        logging(action, 'EXECUTE SENSING ACTION: ~w', [A]),
        write('\t---------> Enter Sensing value, terminate with ".": '),
        read(Sensing), nl.
execute(say(M), _, ok) :- !,
        logging(action, 'SAY: ~w', [M]).
execute(A, _, ok) :-
        logging(action, 'EXECUTE ACTION: ~w', [A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%