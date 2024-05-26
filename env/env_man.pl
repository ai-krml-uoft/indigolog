/*  Environment Manager for the IndiGolog interpreter

    @author Sebastian Sardina 2004 - ssardina@gmail.com

 This code coordinates the exchanges among the various device managers
 and the main IndiGolog interpreter. It is responsible for the execution
 of actions and the handling of sensing outcomes and exogenous actions.
 It does so via TCP/IP sockets.


 This file needs to be consulted in the context of an IndiGolog
 configuration file:

    $ swipl config.pl examples/elevator_sim/main.pl env/env_man.pl

 The following system independent predicates are provided:

 -- initialize_EnvManager -- finalize_EnvManager -- execute_action(A, H,
 T, S) : execute action A of type T at history H and resturn sensing
 outcome S -- exog_occurs(LA)     : return a list LA of exog. actions
 that have occurred (synchronous) -- indi_exog(Action)          :
 asserted whenever exog Action occurred -- set_type_manager(+T)       :
 set the implementation type of the env manager


 The following code is required:

 FROM THE INDIGOLOG MAIN CYCLE:

 -- doing_step : IndiGolog is thinking a step -- abortStep  : abort the
 computation of the current step -- exog_action_occurred(LExoAction) : to
 report a list of exog. actions LExogAction to the top-level

 FROM THE DOMAIN DESCRIPTOR:

 -- server_port(Port) Port to set up the environment manager --
    load_device(Env, Command, Options) for each environemnt to be loaded
    -- how_to_execute(Action, Env, Code) Action should be executed in
    environment Env with using code Code -- translateExogAction(Code,
    Action) Code is action Action -- exog_action(Action) Action is an
    exogenous action

 ELSEWHERE:

 -- logging(T, M)       : report messsage M of type T --
 send_data_socket/2 -- receive_list_data_socket/2
*/
:- ensure_loaded('../lib/utils.pl').

:- dynamic
	got_sensing/2,      % stores sensing outcome for executed actions
	got_exogenous/1,    % stores occurred exogenous actions
	counter_actions/1,  % Carries a counter for each action performed
	dev_data/2,         % (Env name, Pid, Socket) of each device
	executing_action/3, % Stores the current action being executed
	translateExogAction/2,
	translateSensing/3, % Translates sensing outcome to high-level sensing
	how_to_execute/3.   % Defines how to execute each high-level action

name_env(manager).   % We are the "environment manager"
counter_actions(0).  % Counter for (executed) actions


/* A - INITIALIZATION OF DEVICES

 1. Create a TCP socket for the Environment Manager (EM) (saved in
    em_address/1)
 2. Store EM socket info (including pair stream) in em_data/3
 2. Start each application device (via load_devices/1 and load_device/3)
 3. Start the main cycle of the EM (start_env_cycle)

 When each device is started, we wait for them to connect to the EM and
 store its information, including the stream to send/receive data from
 it, in predicate dev_data/2.
*/

initialize(env_manager) :-
	retractall(counter_actions(_)),
	assert(counter_actions(0)),
	load_devices(LDevices),	% get devices to load and how many
    length(LDevices, NoDevices),
	logging(system(1, em), "Openinig EM server socket..."),
	tcp_socket(Socket),
    em_address(Host, Port),
	Address = Host:Port,
	tcp_bind(Socket, Address),
    tcp_listen(Socket, NoDevices),
	tcp_open_socket(Socket, StreamPair),	% stream use for tcp_accept/3
	assert(em_data(Socket, Address, StreamPair)),
	logging(system(2, em), "Loading the following devices: ~w", [LDevices]),
    maplist(start_dev, LDevices), !, % Start each of the devices used
    logging(system(2, em), "Start EM cycle to listen to devices..."),
	start_env_cycle.   % Start the env. manager main cycle

/* Start device manager environment E

   The device manager should be loaded with load_device/3 provided by aplication
   and the information about the device should be stored with dev_data/2.

	A stream pair will be used to send and receive data from the device manager.
*/
start_dev(E) :-
	em_data(SocketEM, AddressEM, _),
	load_device(E, AddressEM, InfoEnv),	% PROVIDED BY APPLICATION!
	tcp_accept(SocketEM, SocketFrom, IP), 		% Wait until the device connects...
	tcp_open_socket(SocketFrom, StreamPair),	% can read/write from/to the device
	stream_pair(StreamPair, ReadStream, _),
	set_stream(ReadStream, alias(E)),	% for better reading with the device name
	logging(system(1, em), "Device ~w initialized at ~w", [E, IP]),
	assert(dev_data(E, [socket(SocketFrom), stream(StreamPair)|InfoEnv])),
	add_stream_to_pool(StreamPair, handle_device(E)).	% register to listen to this stream

dev_stream(E, Stream) :- dev_data(E, L), member(stream(Stream), L).

/* A - FINALIZATION OF DEVICES

	1 - Close all the open device managers
	2 - Terminate EM cycle
	3 - Close EM server socket em_socket
	4 - Report the number of actions that were executed
*/
finalize(env_manager) :-
	logging(system(2, em), "1 - Closing all device managers..."),
    findall(Dev, dev_data(Dev, _), LDev), % Get all current open devices
	maplist(close_dev, LDev), 	% Close all the devices found
	sleep(3), 	% Wait to give time to devices to finish cleanly
	close_stream_pool, !,
	logging(system(2, em), "2 - Terminating EM cycle..."),
	em_data(_, _, StreamPair),
	close(StreamPair),
	catch(wait_for_children, _, true), !,
	logging(system(2, em), "3 - Closing EM server socket..."),
    close(StreamPair), 		% Disconnect server socket
    counter_actions(N),
    logging(system(1, em), "EM completed with *~d* executed actions", [N]).

% keep waiting for all children to finish
% https://www.swi-prolog.org/pldoc/doc_for?object=wait/2
% TODO: maybe send to each env a "exit" and wait for it to finish?
wait_for_children :- wait(PId, S), !,
	logging(system(3, em), "Successful proccess waiting: (~w, ~w)", [PId, S]),
	wait_for_children.
wait_for_children.

% Tell each device to terminate
close_dev(Env) :-
	dev_stream(Env, S),
	write(S, [terminate]).




/*
 We start a thread on em_cycle/1 that waits for data arriving to any of
 the open connections to the device managers. When it receives a message
 (e.g., sensing outcome, exog. action, closing message) it hanldes it.
*/

% this can be used for debugging the EM
% start_env_cycle :- stream_pool_main_loop, !.

start_env_cycle :-
	thread_create(catch(stream_pool_main_loop, E,
			(logging(system(2, em), "EM cycle received exception to finish: ~w", [E]),
			close_stream_pool)), em_thread, [alias(em_thread)]).

end_env_cycle :-
	(	current_thread(em_thread, running)
	-> 	thread_signal(em_thread, throw(finish)) % Signal thread to finish!
	;	true % Already finished (because all devices were closed)
	),
	thread_join(em_thread, _),
	logging(system(1, em), "EM cycle (thread) finished").

% Collect data pending in ready environments
handle_device(Device) :-
	read_term(Device, Term, []),
	(  Term == end_of_file
	-> !
	;  handle_event(Device, Term)
	).
% handle_device(Device) :-
% 	repeat,
% 			read(Device, Term),
% 			(  Term == end_of_file
% 			-> !
% 			;  handle_event(Device, Term), fail
% 			).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% handle_levents(+LE, -LT): handle list of events LE and return their
%                        types in list LT
%
% Up to now, events are either a exogenous events or unknown events.
%
% First handle all events. Then, if there were exogenous event report them
% in a list to top-level cycle with exog_action_occurred/1
%
% 2) Unknown Event: just inform it with message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prioritized serving of a list of occurred events

% 1 - First serve sensing outcomes
handle_levents(L) :-
	member([Sender, [sensing, N, Outcome]], L),
	handle_event([Sender, [sensing, N, Outcome]]),
	fail.

% 2 - Second serve exogenous actions
handle_levents(L) :-
	member([Sender, [exog_action, CodeAction]], L),
	handle_event([Sender, [exog_action, CodeAction]]),
	fail.

% 3 - Third serve other events
handle_levents(L) :-
	member([Sender, [T|R]], L),
	\+ member(T, [sensing, exog_action]),
	handle_event([Sender, [T|R]]),
	fail.

% 4 - Finally, signal exog_action_occurred/1 if there were exogenous actions
handle_levents(_) :-
	findall(ExoAction, got_exogenous(ExoAction), LExoAction),
	LExoAction\=[],                   % There were exogenous actions
	retractall(got_exogenous(_)),
	exog_action_occurred(LExoAction), % Report all the actions to top-level
	fail.

% 5 - Always succeeds as the last step
handle_levents(_).





handle_event(Device, Term) :-
	logging(system(5, em), "Handling event from device ~w: ~w", [Device, Term]).



% handle_event/1: Handle each *single* event
handle_event([_, [sensing, N, OutcomeCode]]) :- !,
	executing_action(N, Action, _),
	(translateSensing(Action, OutcomeCode, Outcome) ->  true ; Outcome=OutcomeCode),
	assert(got_sensing(N, Outcome)),
		logging(system(5),
			["(EM) Sensing outcome arrived for action ",
			(N, Action), " - Sensing Outcome:: ", (OutcomeCode, Outcome)]).

handle_event([_, [exog_action, CodeAction]]):-
	(translateExogAction(CodeAction, Action) -> true ; Action=CodeAction),
    exog_action(Action), !,
	assert(got_exogenous(Action)),
        logging(system(5),
	               ["(EM) Exogenous action occurred:: ", (CodeAction, Action)]).

handle_event([socket(Socket), [_, end_of_file]]) :- !,  % Env has been closed!
        dev_data(Env, _, Socket),                        % remove it
        logging(system(2), ["(EM) Device ", Env, " has reported termination!"]),
        delete_dev(Env).

handle_event([Sender, [Type, Message]]):- !, % The event is unknown but with form
        logging(system(5),
        	["(EM) UNKNOWN MESSAGE! Sender: ", Sender,
                                   " ; Type: " , Type, " ; Message: ", Message]).

handle_event(Data):-                         % The event is completely unknown
        logging(system(5), ["(EM) UNKNOWN and UNSTRUCTURED MESSAGE!:: ", Data]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTION OF ACTIONS SECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tell the corresponding device to execute an action
% how_to_execute/3 says how (which device and which action code) to
%    execute a high-level action
% if the action is a sensing action, it waits until observing its outcome
execute_action(Action, H, Type, N2, Outcome) :-
		% Increment action counter by 1 and store action information
	retract(counter_actions(N)),
	N2 is N + 1,
	assert(counter_actions(N2)), 	% Update action counter
	assert(executing_action(N2, Action, H)), % Store new action to execute
		% Learn how Action should be executed (Env, Code of action)
	map_execution(Action, Env, Code),   % From domain spec
		% Send "execute" message to corresponding device
	logging(system(2, em),
		"Start to execute the following action: ~w", [N2, Action, Env, Code]),
	dev_stream(Env, StreamEnv),
	send_term(StreamEnv, [execute, N2, Type, Code]),
	logging(system(3, em),
		"Action ~w sent to device ~w (waiting for sensing outcome)", [N2, Env]), !,
		% Busy waiting for sensing outcome to arrive (ALWAYS)
	repeat,
	got_sensing(N2, Outcome),
	retract(executing_action(N2, _, _)),
	retract(got_sensing(N2, _)), !,
	logging(system(2, em), 
		"Action ~w completed with outcome: ~w", [[N2, Action, Env, Code], Outcome]).
execute_action(_, _, _, N, failed) :- counter_actions(N).


% Find an adequate device manager that can execute Action and it is active
map_execution(Action, Env, Code) :-
        how_to_execute(Action, Env, Code),
        dev_data(Env, _, _), !.  % The device is running
% Otherwise, try to run Action in the simulator device
map_execution(Action, simulator, Action) :- dev_data(simulator, _, _), !.

% Otherwise, try to run Action in the simulator device
map_execution(Action, _, _) :-
        logging(warning, ["(EM) Action *", Action,
	                      "* cannot be mapped to any device for execution!"]),
	fail.




% Exogenous actions are handled async., so there is no need to handle
% sync. exogenous actions. It is always empty.
exog_occurs([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_man.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%