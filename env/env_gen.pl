/*
        General framework for Device Managers in IndiGolog

 @author Sebastian Sardina (2003) - ssardina@gmail.com

 This files provides the core of a device manager. The user should later  finish the device by providing the implementation of:

        - initialize_interfaces/0
        - finalize_interfaces/0
        - handle_stream/1
        - execute/4

 This file is self-contained (automatically it loads the required libraries).
 It should be called as follows:

   eclipse host=<HOST> port=<PORT> -b env_rcx.pl -e start

 where HOST/PORT is the address of the environment manager socket.

 This file defines the following predicates:

 -- start      : initialization of the environment (called when loaded)
 -- finalize   : finalization of the environment (called when exiting)
 -- main_dir/1 : defines the IndiGolog root directory
 -- report_exog_event(+A, +M)
 		report exogenous event A with message M to the environment manager
 -- report_sensing(+A, +N, +S, +M)
		report sensing outcome S from action A with number N and message M


 plus handle_stream(env_manager) for handling messages from the
 environment manager.

 Required:

    -- ECLIPSE compatibility library


 In order to complete a device manager the user should implement:

  -- name_dev/1                 : mandatory
  -- initialize_interfaces      : mandatory
  -- finalize_interfaces        : mandatory
  -- execute/4                  : mandatory
  -- handle_steam/1             : as needed
  -- listen_to/3                : as needed
*/
% :- initialization(start, main).

:- dynamic terminate/0,    % To signal when the environment should quit
           listen_to/3.    % Streams and Sockets to look after

% https://www.swi-prolog.org/pldoc/man?section=optparse
:- use_module(library(optparse)).
opts_spec(env_gen,
        [ [opt(host), shortflags([h]), longflags(['host']), type(atom),
                help('Host of the environment manager')],
        [opt(port), shortflags([p]), longflags(['port']), type(integer),
                help('Port of the environment manager')],
        [opt(debug), shortflags([d]), longflags(['debug']), type(integer), default(100),
                help('Debug level')]], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


wait_until_close(5). % how many seconds to wait until closing the device manager


start :- catch(run, E, (logging(error, "Error in device manager: ~w", [E]), trace)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% START OF STANDARD SECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Run at the beginning of the environment setting
% It initializes the communication with the environment manager and
% it initializes every source of input (rcx, tcl/tk, etc.)
run :- name_dev(EnvId),
        logging(info(1, gen), "Initializing environment ~w", [EnvId]),
                % 1 - Process CLI options
        current_prolog_flag(argv, Argsv),
        opts_spec(env_gen, OptsSpec, PositionalArgs),
        opt_parse(OptsSpec, Argsv, Opts, PositionalArgs, [output_functor(cli_args)]), !,
        writeln(Opts),
        maplist(assert, Opts),
                % 2 - Set debug level
        (cli_args(debug, DebugLevel) -> true ; DebugLevel = 10),
        set_option(log_level, DebugLevel),
        logging(info(1, gen), "Set log level to ~d", [DebugLevel]),
                % 3 - Obtain Host and Port number of env. manager from command
        logging(info(1, gen), "Setting socket connection with EM"),
        cli_args(host, Host),
        cli_args(port, Port),
        sleep(1),  % Give time to EM to wait for us
                % 4 - Connect to EM via network and extract read/write stream
                %  read/write stream to talk to EM  (send actions, receive exog events and sensing)
        tcp_connect(Host:Port, StreamPair, []),
        assert(env_manager(Host:Port, StreamPair)),
        stream_pair(StreamPair, ReadStream, WriteStream),
        set_stream(ReadStream, alias(em_read_stream)),
        set_stream(WriteStream, alias(em_write_stream)),
        add_stream_to_pool(StreamPair, handle_stream(env_manager)),
                % 5 - Initialize different interfaces
        logging(info(1, gen), "Initializing required interfaces..."),
        initialize_interfaces, !,  %%%%%%%%%%% USER SHOULD IMPLEMENT THIS!
                % 6 - Run the main cycle
        logging(info(1, gen), "Starting device manager main cycle..."), !,
        stream_pool_main_loop,
                % 7 - Terminate interfaces
        logging(info(1, gen), "Finalizing domain interfaces..."), !,
        finalize_interfaces,     %%%%%%%%%%% USER SHOULD IMPLEMENT THIS!
        close(StreamPair),
        halt_device.
run :- logging(error, "For some reason the environment has stopped"), halt_device.

halt_device :-
        (wait_until_close(Secs) -> true ; Secs = 5),
        logging(info(1, gen), "Halting device in ~d seconds...", [Secs]), !,
        sleep(Secs), 		% Hold for Seconds and then close
        halt.

% halt device after waiting for some seconds (so that one can read debug info)
break_device :-
        logging(info(1, gen), "Device manager breaking.."),
	break.


% order termination of the device manager
order_device_termination :- terminate -> true ; assert(terminate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLERS FOR INPUT ON STREAMS (event manager and interfaces)
%
% This section implements how each stream is handled when input arrives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic handle_stream/1.  % NEEDED BECAUSE IT MAY BE DEFINED IN 2 FILES!
:- discontiguous handle_stream/1.
:- multifile(handle_stream/1).

% Standard handler for the event manager stream:
% called when the environment manager sent something
% usually, there is no need to modify it, one should implement execute/3
handle_stream(env_manager) :-
        logging(info(4, gen), "Handling data from EM"),
        read_term(em_read_stream, Data, []),
        get_code(em_read_stream, 32),   % discard the space after the full stop
        logging(info(3, gen), "Received data from EM: ~w", [Data]), !,
        handle_data(env_manager, Data).

handle_data(env_manager, Data) :-
        member(Data, [finish, end_of_file]), !,
        logging(info(2, gen), "Termination requested from EM: ~w", [Data]),
        close_stream_pool.
handle_data(env_manager, execute(N, Action)) :- !,
        logging(info(3, gen), "About to execute action ~d: ~w", [N, Action]),
        (       execute(Action, N, SR)     % PROVIDED BY DOMAIN!
        ->      logging(action, "Action ~w executed with outcome: ~w", [N, [Action, SR]])
        ;       logging(action, "Action ~w FAILED to execute (assumed failed)", [[Action, N]]),
                SR = failed
        ),
        report_sensing(Action, N, SR).
handle_data(env_manager, Data) :-
        logging(warning, "No rule for handling data: ~w", [Data]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOOL FOR REPORTING EXOGENOUS EVENT AND SENSING TO THE ENVIRONMENT MANAGER
%
%  report_exog_event(A, M)
%       report exogenous event A and message M to the environment manager
%  report_sensing(A, N, S, M)
%       report sensing outcome S for action A with number N and print
%       message M
%
% The device managers use this tool to report the occurrence of exogenous
% events/actions and action sensing outcomes.
% Message is a message that should be printed in the device manager output.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
report_exog(A) :-
        logging(exogaction, "Exogenous action occurred: ~w", [A]),
        send_term(em_write_stream, exog_action(A)).

report_sensing(A, N, SR) :-
        logging(sensing, "Sending sensing action ~d to EM: ~w", [N, [A, SR]]),
        send_term(em_write_stream, sensing(A, N, SR)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
