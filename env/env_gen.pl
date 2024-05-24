/*
        General framework for Device Managers in IndiGolog

 @author Sebastian Sardina (2003) - ssardina@gmail.com

 This files provides the core of a device manager. The user should later  finish the device by providing the implementation of:

        - initializeInterface/0
        - finalizeInterface/0
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
 -- change_action_state(+A, +N, +State, +Sensing, +LExogEvents):
         change the state of Action-N to State, set its Sensing and the list of
         exogenous events generated due to the action



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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_until_close(5). % how many seconds to wait until closing the device manager




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% START OF STANDARD SECTION %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start :-
        current_prolog_flag(argv, Argsv),
        opts_spec(env_gen, OptsSpec, PositionalArgs),
        opt_parse(OptsSpec, Argsv, Opts, PositionalArgs, [output_functor(cli_args)]), !,
        writeln(Opts),
        maplist(assert, Opts),
        catch_call(start2, "Main cycle for device manager got exception", fail).
start :- logging(error, "For some reason the environment has stopped"), halt_device.

% Run at the beginning of the environment setting
% It initializes the communication with the environment manager and
% it initializes every source of input (rcx, tcl/tk, etc.)
start2 :-
        name_dev(EnvId),
        logging(system(1), "Initializing environment ~w", [EnvId]),
        % 1 - Set debug level
        (cli_args(debug, DebugLevel) -> true ; DebugLevel = 10),
        set_option(log_level, DebugLevel),
        logging(system(1), "Set log level to ~d", [DebugLevel]),
        % 2 - Obtain Host and Port number of env. manager from command
        logging(system(1), "Setting socket connection with EM"),
        cli_args(host, Host),
        cli_args(port, Port),
        sleep(3),  % Give time to EM to wait for us
        tcp_connect(Host:Port, StreamPair, []),
        assert(env_manager(Host:Port, StreamPair)),
        assert(listen_to(env_manager, StreamPair)),
        % 3 - Initialize different interfaces
        logging(system(1), "Initializing required interfaces..."),
        initialize_interfaces,   %%%%%%%%%%% USER SHOULD IMPLEMENT THIS!
        % 4 - Run the main cycle
        logging(system(1), "Starting main cycle"), !,
        main_cycle,
        % 5 - Terminate interfaces
        logging(system(1), "Finalizing domain interfaces..."), !,
        finalize_interfaces,     %%%%%%%%%%% USER SHOULD IMPLEMENT THIS!
        logging(system(1), "Device manager totally finished; about to halt..."),
        close(StreamPair),
        halt_device.

halt_device :-
        (wait_until_close(Seconds) -> true ; Seconds = 5),
        sleep(Seconds), 		% Hold for Seconds and then close
        halt.

% halt device after waiting for some seconds (so that one can read debug info)
break_device :-
        logging(system(1), "Device manager breaking.."),
	break.


% MAIN CYCLE: Wait for data to arrive from data comming from the
%             environment manager or any interface that was initialized
%             and stored in listen_to/3.
%             Here we wait for the tcl/tk pipe and the env_manager socket
%
% listen_to(Type, Id, X) means that X should be checked at every cycle
main_cycle :- terminate, !.
main_cycle :-
        findall(Stream, listen_to(Stream, _), LStreams),
        logging(system(3), "Waiting the following streams: ~w", [LStreams]),
        wait_for_input(LStreams, ReadyStreams, infinite),
        logging(system(3), "Streams ready: ~w", [ReadyStreams]),
        maplist(handle_stream, ReadyStreams),
        main_cycle.

% order termination of the device manager
order_device_termination :- terminate -> true ; assert(terminate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLERS FOR INPUT ON STREAMS (event manager and interfaces)
%
% This section implements how each stream is handled when input arrives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic handle_stream/1.  % NEEDED BECAUSE IT MAY BE DEFINED IN 2 FILES!
:- discontiguous handle_stream/1.
:- multifile(handle_stream/1).

% Standard handler for the event manager stream:
% called when the environment manager sent something
% usually, there is no need to modify it, one should implement execute/3
handle_stream(env_manager) :-
        logging(system(3), "Handling data on env_manager"),
        receive_data_socket(env_manager, [_, Data]),
	((Data = [terminate] ; Data = [system, end_of_file]) ->
             logging(system(2), ["Termination requested. Reason: ", Data]),
             order_device_termination
        ;
         Data = [execute, N, Type, Action] ->
	     change_action_state(Action, N, orderExecution, null, []),
	     logging(system(3), ["About to execute *", (Action, N), "*"]),
             (execute(Action, Type, N, S) ->
		logging(system(3), ["Action *", (Action, N), "* executed with outcome: ", S])
	     ;
		logging(error, ["Action *", (Action, N), "* could not execute (assumed failed)"]),
		S=failed
	     ),
%	     change_action_state(Action, N, finalExecution, S, []),
             % Report the sensing if it was not "null" (null=not available yet)
             % If it was null, then the user should make sure that the
             % sensing outcome will be eventually reported to the manager
             (S \=null -> report_sensing(Action, N, S, _) ; true)
        ;
             logging(warning, ["Uknown message from Manager: ", Data])
        ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
report_exog_event(A, Message) :- var(Message), !,
        logging(exogaction, "Exogenous action ~w occurred", [A]),
        send_data_socket(env_manager, [exog_action, A]).
report_exog_event(A, Message) :-
        logging(exogaction, Message),
        send_data_socket(env_manager, [exog_action, A]).

report_sensing(A, N, S, Message) :- var(Message), !,
        logging(sensing, ["Sending sensing to manager:  ", (A, N, S)]),
        send_data_socket(env_manager, [sensing, N, S]).
report_sensing(_, N, S, Message) :-
        logging(sensing, Message),
        send_data_socket(env_manager, [sensing, N, S]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% change_action_state(A, N, State, Sensing, LExogEvents):
%    change the state of Action-N to State, set its Sensing and the list of
%    exogenous events generated due to the action
%
% State can be:
%      orderExecution  : order of execution received but still not executed
%      finalExecute    : action finished execution with sensing outcome S
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stores action number, state, sensing outcome, and associated exogenous events
:- dynamic actionState/4.

change_action_state(_, N, orderExecution, _, _) :- !,
	assert(actionState(N, orderExecution, null, [])).
change_action_state(_, N, State, Sensing, LExog) :-
	retract(actionState(N, OldState, OldSensing, OldExog)),
	(var(State)   -> NewState=OldState ; NewState=State),
	(var(Sensing) -> NewSensing=OldSensing ; NewSensing=Sensing),
	(var(LExog)   -> NewExog=OldExog ; append(LExog, OldExog, NewExog)),
	assert(actionState(N, NewState, NewSensing, NewExog)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_gen.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%