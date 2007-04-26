%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/env_man.pl
%
%  AUTHOR : Sebastian Sardina (2004-2006)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent code (SWI or ECLIPSE Prolog)
%  TESTED : ECLiPSe 5.3 on RedHat Linux 6.2-9.0
%                SWI Prolog 5.2.8 under RedHat Linux 6.2/9.0
%
% DESCRIPTION: The environment manager deals with many multiple devices
% by communicating with them via TCP/IP sockets
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             September, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000-2002 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following system independent predicates are provided:
%
% -- initializeEM
% -- finalizeEM
% -- execute_action(A, H, T, S) : execute action A of type T at history H
%                                 and resturn sensing outcome S
% -- exog_occurs(LA)		: return a list LA of exog. actions that
%				  have occurred (synchronous)
% -- indi_exog(Action)          : asserted whenever exog Action occurred
% -- set_type_manager(+T)       : set the implementation type of the env manager
%
%
% The following code is required:
%
% FROM THE INDIGOLOG MAIN CYCLE:
%
% -- doing_step : IndiGolog is thinking a step
% -- abortStep  : abort the computation of the current step
% -- exog_action_occurred(LExoAction) 
%               : to report a list of exog. actions LExogAction to the top-level
%
% FROM THE DOMAIN DESCRIPTOR:
%
% -- server_port(Port)       
%	Port to set up the environment manager
% -- load_environment(Env, Command, Options) 
%     	for each environemnt to be loaded
% -- how_to_execute(Action, Env, Code)
%       Action should be executed in environment Env with using code Code
% -- translateExogAction(Code, Action)  
%	Code is action Action
% -- exog_action(Action) 
%	Action is an exogenous action
%
% ELSEWHERE:
%
% -- report_message('EM', T, M)       : report messsage M of type T
% -- send_data_socket/2
% -- receive_list_data_socket/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 
	got_sensing/2,      % stores sensing outcome for executed actions
	got_exogenous/1,    % stores occurred exogenous actions
	counter_actions/1,  % Carries a counter for each action performed
	env_data/3,         % (Env name, Pid, Socket) of each device
	executing_action/3, % Stores the current action being executed
	translateExogAction/2,
	translateSensing/3, % Translates sensing outcome to high-level sensing
	how_to_execute/3,   % Defines how to execute each high-level action
	type_manager/1,     % Defines the implementation type of the manager
	server_port/1,
	server_host/1.	    % Host and port for the environment manager

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_env(manager).   % We are the "environment manager"
counter_actions(0).  % Counter for (executed) actions

set_option('type_em : set type of environment manager: thread or signal').
set_option(type_em, T) 	:- 
	set_type_manager(T),
	report_message('EM', system(0), ['** EM fixed to ', T]).


% Set the type of the environment manager: thread or signal based
set_type_manager(T) :- 
	atom(T),
	member(T, [thread,signal]),
        report_message('EM', system(2), 
                       ['Setting environment manager type to: ',T]), 
        retractall(type_manager(_)), 
        assert(type_manager(T)).
set_type_manager(_) :- 
        report_message('EM', warning, 'Type of env. manager cannot be set!').

type_manager(thread). % Default execution for the env. manager


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INITIALIZATION AND FINALIZATION PART
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% A - Initialize environment manager
initializeEM :- 
          report_message('EM', system(2), '1 - Performing DB initializations...'),
	retractall(counter_actions(_)),
	assert(counter_actions(0)),
        retractall(env_data(_, _, _)),
	retractall(executing_action(_,_,_)),
	retractall(got_sensing(_,_)),
	retractall(got_exogenous(_)),
	(type_manager(Type), finish_env_cycle(Type) ->  true ; true), !,
          report_message('EM', system(1), '2 - Openinig server-input socket...'),
        socket(internet, stream, em_socket), % signal when data comes
		% Build the Address where the manager will be listeling
	(server_host(ServerHost2) -> true ; gethostname(ServerHost2)),
        string_to_atom(ServerHost2, ServerHost),
        server_port(ServerPort),
        Address=ServerHost/ServerPort,
        bind(em_socket, Address), !,
          report_message('EM', system(1),'3 - Loading different devices...'),
        setof(Env, A^B^load_device(Env,A, B), LEnv),
        (LEnv=[] ->
               report_message('EM', warning,'No devices defined to load!') 
	;
               true
	),
        length(LEnv, LengthLEnv),
        listen(em_socket, LengthLEnv), !,
        (start_env(LEnv, Address) ->
		true
	;
		report_message('EM', error,'Cannot start all environment managers'),
		finalizeEM,
		fail
	), !, % That is it, ready to start the main cycle
          report_message('EM', system(2),'4 - Start EM cycle...'),
        type_manager(Type),
	start_env_cycle(Type).   % Start the env. manager main cycle


% The finalization of the EM involves:
%	1 - Close all the open device managers
%	2 - Terminate EM cycle
%	3 - Close EM server socket em_socket
%	4 - Report the number of actions that were executed
finalizeEM :- 		% Instruct all open environments to close
        report_message('EM', system(2),'1 - Instruct all open environments to close...'),
        setof(Dev, X^Y^env_data(Dev, X, socket(Y)), LDev), % Get all current open devices
	close_dev(LDev),		% Close all the devices found
	sleep(3),			% Wait to give time to devices to finish cleanly
        report_message('EM', system(2),'1 - All devices where informed to terminate'),
	fail.	  
finalizeEM :-
	report_message('EM', system(2), '2 - Terminating EM cycle...'),
        type_manager(Type),
	finish_env_cycle(Type),   % Terminate main env. manager cycle
	report_message('EM', system(2), '2 - EM cycle finished...'),
	fail.
finalizeEM :- 		% Delete every running environment that is still open
        report_message('EM', system(2),'3 - Instruct all open environments to close...'),
        setof(Dev, X^Y^env_data(Dev, X, Y), LDev), % Get all current open devices
	delete_dev(LDev),			% Delete list of environments
        report_message('EM', system(2),'3 - All open environments were closed/deleted...'),
	fail.	  
finalizeEM :-
	report_message('EM', system(2),'4 - Closing EM server socket...'),
        (close_socket(em_socket) -> fail ; fail). % Disconnect EM server socket fail.
finalizeEM :-
        counter_actions(N),
        report_message('EM', system(1),
        	['Finalization of EM completed with *', N, '* executed actions.']).




% B - Initialization and Finalization of the environment manager main cycle
%		start_env_cycle/0  : starts EM cycle
%		finish_env_cycle/0 : terminates EM cycle
%       The EM cycle basically waits for data arriving to any of the open
%       connections to the device managers. When it receives a message
%	(e.g., sensing outcome, exog. action, closing message) it hanldes it.

% B.1 THREAD IMPLEMENTATION (multithreading Prologs like SWI)
%	 There is a separated thread to handle incoming data. The thread
%	 should BLOCK waiting for data to arrive
start_env_cycle(thread) :- 
	thread_create(catch(em_cycle_thread,E,
		(E=finish -> 
		 	report_message('EM', system(2),'EM cycle finished successfully')
		; 
		 	report_message('EM', error,['EM Thread Error:',E])
		)),_,[alias(em_thread)]).
em_cycle_thread :- em_one_cycle(block), !, em_cycle_thread.
em_cycle_thread :-	 % if em_one_cycle/1 has nothing to wait, then just terminate
	 report_message('EM', system(2),'EM cycle finished, no more streams to wait for...').
	

finish_env_cycle(thread) :-
	(current_thread(em_thread, running) -> 
		% Signal thread explicitely to finish!
		thread_signal(em_thread, throw(finish))  
	;
		% The thread has already finished (because all devices were closed)
		true
	), 
	catch(thread_join(em_thread,_),E,
		report_message('EM', warning,['Cannot join EM thread: ',E])),
	report_message('EM', system(3),'Environment cycle (thread) finished').  

% B.2. SIGNAL IMPLEMENTATION (interrputs with ECLIPSE)
%
%
start_env_cycle(signal) :- 
	report_message('EM', system(1),'3 - Registering io interrupt handler....'),
        set_interrupt_handler(io, handle_io/0),
	report_message('EM', system(1),'Env. Manager initialization successful!'). 

finish_env_cycle(signal) :- 
	set_interrupt_handler(io, default/0).  % Reset io handler

% The handle_io/0 is called when new data has arrived from one of the
% active devices.
% The hanlder collects the sockets that have information to be read,
% reads all that data from them (get_events_from_env/2) and then handle
% the list (of events read from the ready env) read using handle_events/1
handle_io(_) :- handle_io.  % Wrapper for SWI that uses 1-ary signal hanlder
handle_io :- em_one_cycle(0).        







% C - em_one_cycle/1 : THIS IS THE MOST IMPORTANT PREDICATE FOR THE EM CYCLE
%
% 	em_one_cycle(HowToWait) does 1 iteration of the waiting cycle and
%	waits HowMuchToWait (seconds) for incoming data from the devices
% 	If HowMuchToWait=block then it will BLOCK waiting... (good for threads)
em_one_cycle(HowMuchToWait) :-
	report_message('EM', system(5),'Waiting data to arrived at env. manager (block)'),
	% Get all the read-streams of the environments sockets
	setof(Socket, X^Y^env_data(X, Y, socket(Socket)), ListSockets),
	% Check which of these streams have data waiting, i.e., the "ready" ones
	report_message('EM', system(5), ['Blocking on environments:: '|ListSockets]),
	select(ListSockets, HowMuchToWait, ListSocketsReady),   !, % BLOCK or wait?
	% Get back the name of the environments of these "ready" streams
	setof(Env, S^X^(member(S, ListSocketsReady),
			env_data(Env, X, socket(S))), ListEnvR),
	report_message('EM', system(5), ['Handling messages in devices:: '|ListEnvR]),
	% Next, read all the events waiting from these devices
	get_events_form_env(ListEnvR, ListEvents),
	% Finally, handle all these events
	handle_levents(ListEvents).

% Given a list of devices that have information on their sockets
% collect all the data from them
get_events_form_env([], []).
get_events_form_env([Env|LEnv], TotalListEvents) :-
        env_data(Env, _, socket(SocketEnv)),
        catch(receive_list_data_socket(SocketEnv, LEventsEnv),E,
		(report_message('EM', error,['Could not read anything from environment ',
			Env,'===> ',E]),
		 LEventsEnv=[])
	),
        get_events_form_env(LEnv, RestEvents),
        append(LEventsEnv, RestEvents, TotalListEvents).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% handle_levents(+LE,-LT): handle list of events LE and return their 
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
	member([Sender,[sensing, N, Outcome]], L),
	handle_event([Sender,[sensing, N, Outcome]]),
	fail.

% 2 - Second serve exogenous actions
handle_levents(L) :-
	member([Sender,[exog_action, CodeAction]], L),
	handle_event([Sender,[exog_action, CodeAction]]),
	fail.

% 3 - Third serve other events
handle_levents(L) :-
	member([Sender,[T|R]], L),
	\+ member(T, [sensing, exog_action]),
	handle_event([Sender,[T|R]]),
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



% Handle each *single* event
handle_event([_, [sensing, N, OutcomeCode]]) :- !, 
        executing_action(N, Action, _),
	(translateSensing(Action, OutcomeCode, Outcome) -> 
		true 
        ; 
	        Outcome=OutcomeCode
	),
	notify_sensing(N, Outcome),
        report_message('EM', system(5),
	               ['Sensing outcome arrived for action ',
		        (N, Action), ' - Sensing Outcome:: ',(OutcomeCode,Outcome)]).

% Notify that action N has received sensing outcome Outcome
notify_sensing(N, Outcome) :-
	type_prolog(swi), !,
	thread_send_message(main,got_sensing(N, Outcome)).	
notify_sensing(N, Outcome) :-
        assert(got_sensing(N, Outcome)).


handle_event([_, [exog_action, CodeAction]]):- 
	(translateExogAction(CodeAction, Action) -> 
		true 
	; 
	        Action=CodeAction
	),
        exog_action(Action), !,
	assert(got_exogenous(Action)),
        report_message('EM', system(5),
	               ['Exog. action occurred:: ',(CodeAction, Action)]).

handle_event([socket(Socket), [_, end_of_file]]) :- !,  % Env has been closed, remove it!
        env_data(Env, _,socket(Socket)),  
        delete_dev([Env]),
        report_message('EM', system(5),['Device ',Env,' has terminated!']).

handle_event([Sender, [Type, Message]]):- !, % The event is unknown but with form
        report_message('EM', system(5), ['UNKNOWN MESSAGE! Sender: ',Sender,
                                   ' ; Type: ' ,Type,' ; Message: ',Message]).

handle_event(Data):-                         % The event is completely unknown
        report_message('EM', system(5), ['UNKNOWN MESSAGE!:: ', Data]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% START AND CLOSE DEVICE MANAGERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start_env(LEnv, Address) : starts a list of device managers
%
%  each device is started in a separate xterm window. We pass the 
%  Address so that the device knows where to send its data
%  Also we receive from the device its address to know where to
%  send data to it. 
%  env_data/3 stores the Pid and Address of each device started
start_env([], _).
start_env([Env|LEnv], Address) :-
	report_message('EM', system(4), ['Will start environment ', Env]),
	Address = Host/Port,
	load_device(Env, Command, [Host,Port]), 
	report_message('EM', system(4), 
	['About to start environment ', Env, ' with the following command: ', Command]),
	fork_environment(Env,Command, Pid),	
	assert(env_data(Env, Pid, none)),	% At this point the environment may exists with Pid
	(type_manager(thread) ->	% Wait TCP connection from device manager
		accept(em_socket, From, Env) 	 	% SWI
	;
		accept(em_socket, From, sigio(Env))	% ECLIPSE
	),
	retract(env_data(Env, Pid, none)),
	assert(env_data(Env, Pid, socket(Env))),	% Store the environnment stream 
	report_message('EM', system(1), ['Device ', Env, ' initialized at: ', From]),
	start_env(LEnv, Address).

fork_environment(_Env,(Command,LArgs), Pid) :- !,
	call_to_exec(unix, (Command,LArgs), Command2), % Select right command for exec/1
	exec_group(Command2, [], Pid).


% Instruct a list of environment to close by sending the 'terminate' message
close_dev([]).
close_dev([Env|LEnv]) :-	% Env has an open socket
	env_data(Env,_Pid,socket(SocketEnv)), 
	report_message('EM', system(4), ['Will instruct close of environment: ', Env]),
	send_data_socket(SocketEnv, [terminate]),  % Tell device to terminate
	% (delete_dev([Env]) -> true ; true),   % not needed, will be deleted automatically
	close_dev(LEnv).
close_dev([_|LEnv]) :- close_dev(LEnv).	% Env does not have an open socket


% Delete all information of a list of environments
% delete_dev/1 is called automatically when the device has reported to be terminated
delete_dev([]).
delete_dev([Env|LEnv]) :-
	retract(env_data(Env, Pid, SocketEnv)),
	report_message('EM', system(1), ['Deleting environment *',Env,'*']),
	timeout(proc_wait(Pid, S),10,
		(proc_kill(Pid), proc_wait(Pid,S))),	% wait for Pid or kill it!
	!,
	(ground(S) -> true ; S=free),
	report_message('EM', system(1),
		['Environment *',Env,'* deleted! - Wait result: ',(Pid, S)]),
	(SocketEnv=socket(Socket) -> close_socket(Socket) ; true),  % Disconnect server socket
	delete_dev(LEnv).



% Tries to close socket X but catches the exception if not possible
close_socket(X) :-
	catch(close(X),E,report_message('EM', warning,['Cannot close socket ',X,'--> ',E])).



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
	N2 is N+1,
	assert(counter_actions(N2)),	% Update action counter
	assert(executing_action(N2, Action, H)), % Store new action to execute
		% Check that the Action is a real concrete one
	ground(Action),
		% Learn how Action should be executed (Env, Code of action)
	map_execution(Action, Env, Code),   % From domain spec
		% Send "execute" message to corresponding device
	report_message('EM', system(2), 
		['Start to execute the following action: ',(N2, Action, Env, Code)]),!,
	(Env=internal ->
		Outcome=ok
	;
		env_data(Env, _, socket(SocketEnv)),
		send_data_socket(SocketEnv, [execute, N2, Type, Code]),
		report_message('EM', system(3),
			['Action ',N2,' sent to device ',Env,
				' - Waiting for sensing outcome to arrive']),!,
		% Busy waiting for sensing outcome to arrive (ALWAYS)
		wait_for_sensing(N2, Outcome),
		retract(executing_action(N2, _, _))
	),
	report_message('EM', system(2), ['Action *', (N2, Action, Env, Code), 
					'* completed with outcome: ',Outcome]), !.
execute_action(Action, _, _, N, failed) :- 
	\+ ground(Action),
	counter_actions(N),
	report_message('EM', error, ['Nonground action cannot be executed: ',(N, Action)]).	
execute_action(Action, _, _, N, failed) :- 
	report_message('EM', error, ['Action cannot be executed: ',(N, Action)]),
	counter_actions(N).

% Wait for action N to get sensing Outcome
wait_for_sensing(N, Outcome) :- type_prolog(swi), !,		% Via thread messages
		thread_get_message(got_sensing(N, Outcome)).	% Block if necessary
wait_for_sensing(N, Outcome) :- 	% Busy waiting (inefficient)
		repeat,   
		got_sensing(N, Outcome),
		retract(got_sensing(N, Outcome)).



% Find an adequate device manager that can execute Action and it is active
map_execution(Action, Env, Code) :-
        how_to_execute(Action, Env, Code),
        env_data(Env, _, _), !.  % The device is running 
% Otherwise, try to run Action in the simulator device 
map_execution(Action, simulator, Action) :- env_data(simulator, _, _), !.
% If no simulator is running, treat is as an internal action (always succeeds)
map_execution(Action, internal, Action).

% Otherwise, try to run Action in the simulator device 
map_execution(Action, _, _) :-
        report_message('EM', warning, ['Action *', Action, 
	                      '* cannot be mapped to any device for execution!']),
	fail.

% Exogenous actions are handled async., so there is no need to handle
% sync. exogenous actions. It is always empty.
exog_occurs([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/env_man.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%