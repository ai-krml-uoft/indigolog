:- module(utils, [
        join_to_string/2,
        join_to_string/3,
        any_to_string/2,
        % 3 - OS TOOLS
        exec/2,
        exec/3,
        exec_group/3,
        set_interrupt_handler/2,
        current_interrupt/2,
        get_interrupt_handler/3,
        stime/1,
        cputime/1
          ]).


% join_to_string(+List, +Sep, -String):
%    String is concatenated elements of List separated by Sep
%   generalization of https://www.swi-prolog.org/pldoc/man?predicate=atomics_to_string/2
%   to handle terms
join_to_string(L, String) :- join_string(L, "", String).
join_to_string(L, Sep, String) :- maplist(term_string, L, L2), atomics_to_string(L2, Sep, String).


% Convert anything into a string: generalizes term_string/2 to allow vars
any_to_string(A, "_Var") :- var(A), !.
any_to_string(A, A) :- string(A), !.
any_to_string(A, S) :- atom(A), atom_string(A, S), !.
any_to_string(A, S) :- term_string(A, S).




/*
3 - OPERATING SYSTEM TOOLS
-- cputime(?Time)
-- stime(?Time)
       Succeeds if Time is the elapsed user cpu time in seconds.
Management of interrupts/signals as ECLIPSE does:
-- set_interrupt_handler/2
-- current_interrupt/2
-- get_interrupt_handler/3
Operating system EXEC utilities:from ECLIPSE
-- exec(+Command, ?Streams)
       A child process Command is forked, its standard streams are
       connected to Streams and the ECLiPSe process waits until it terminates.
-- exec(+Command, ?Streams, -Pid)
       A child process Command is forked, its standard streams are
       connected to Streams and its process ID is Pid.
    Description (adapted from ECLIPSE manual)

This predicate is used to fork a child process and to set up pipes to its
standard streams. After the process is forked, execution continues normally,
without waiting for the child to terminate.
By specifying the Streams argument it is possible to connect to the
process' standard streams. The form of
Streams is [Stdin, Stdout, Stderr]. Stderr is ignored in the current
implementation.
If some of these streams are specified and
not null, a pipe is opened which connects the standard stream of the child
process with the specified stream, e.g. Stdin must be an output stream
because it is connected to the standard input of the child process.
Specifying a null stream means that no pipe is set up for this stream.
Stdout can also be specified as sigio(Stream) (BSD systems only). In this
case a pipe is set up to the stream Stream and in addition the pipe is
instructed to send the signal io each time new data appears in it. In this way
the two processes can communicate in a truly asynchronous way. When one
process sends data to the other one, the interrupt handler is invoked and it
can read and process the data. When it finishes, it can continue where it was
interrupted.
If one wants to run a command with the shell, use: sh('-c', Command)
After forking the process, Pid is unified with its process ID, which can be
used e.g. in wait/2 or kill/2. If the exec system call in the child process
failed, the child exits with status 128 + errno.
-- exec_group(+Command, ?Streams, ?Pid)
       A child process Command is forked in a new process group, its
       standard streams are connected to Streams and its process ID is Pid.
    (NOTE: currently, equivalent to exec/3)
-- system(+ShellCommand)
-- sh(+ShellCommand)
       The string or atom ShellCommand is passed as a command to the
       operating system, and the command is executed there
*/

% Returns the CPU time
stime(T)   :- T is cputime.
cputime(T) :- T is cputime.   % ECLIPSE compatibility

% Interrupts managements in the way ECLIPSE does
set_interrupt_handler(Signal, default/0)  :- !, on_signal(Signal, _, default).
set_interrupt_handler(Signal, event/1)    :- !, on_signal(Signal, _, throw).
set_interrupt_handler(Signal, N/0)        :- on_signal(Signal, _, N).
current_interrupt(Id, Name)               :- current_signal(Name, Id, _).
get_interrupt_handler(IntId, PredSpec, _) :- current_signal(_, IntId, PredSpec).


% Implementation of exec/2, exec/3 and exec_group/3 (Compatible with ECLIPSE)
%
% A child process Command is forked. Its standard streams are connected to
% [StdIn, StdOut, _] and its process ID is Pid.
% (This is a partial implementation of ECLIPSE exec_group/3)
% Differences: does not run Command in a different process group and it
%              does not set the error channel

% exec_group/3 from ECLIPSE to attach Streams and return Pid
% NOTE: For now I cannot separate the child
exec_group(C, Streams, Pid) :- exec(C, Streams, Pid).

% exec/2: no return of Pid so we wait on process
exec(Command, Streams) :-
 	 exec(Command, Streams, Pid),
	 wait(Pid, _).

% exec/3: general case exec(Command, Streams, PiD)
%  fork a child process Command, its standard streams are connected to Streams and its process ID is Pid.
exec(Command, [], P) :- exec(Command, [null, null, null], P).
exec(Command, [StreamOut], P) :- exec(Command, [StreamOut, null, null], P), wait(P, _).
exec(Command, [StreamOut, StreamIn], P) :- exec(Command, [StreamOut, StreamIn, null], P).

% Handle the case for sigio(S)
exec(Command, [StreamOut, SIn, _], Pid) :-
    \+ var(SIn), SIn = sigio(StreamIn), !,
    exec(Command, [StreamOut, StreamIn2, _], Pid),
    register_stream_sigio(StreamIn2, StreamIn3),
    register_stream_name(StreamIn3, StreamIn).

% Handle the general case
exec(Command, [StreamOut, StreamIn, _], Pid) :-
    (   StreamOut == null
    ->  true
    ;   pipe(CGIIn, StreamOut2), register_stream_name(StreamOut2, StreamOut)
    ),
    (   StreamIn == null
    ->  true
    ;   pipe(StreamIn2, CGIOut), register_stream_name(StreamIn2, StreamIn)
    ),
    fork(Pid),
    (   Pid == child
    ->  % detach_IO % may this work to detach the child ?
        (   StreamOut == null
        -> true
        ; (close(StreamOut), dup(CGIIn, 0),  close(CGIIn))   % stdin
        ),
        (   StreamIn  == null
        ->  true
        ;
            close(StreamIn), dup(CGIOut, 1), close(CGIOut)   % stdout
        ),
        % exec('/bin/sh '('-c', Command))
        exec(Command)
    ;   (StreamOut == null -> true ; close(CGIIn)),
        (StreamIn  == null -> true ; close(CGIOut))
    ).


system(ShellCommand) :- shell(ShellCommand).
sh(ShellCommand)     :- shell(ShellCommand).



:- dynamic sigio/3.  % Stores streams that we are watching for IO
                     % (Source, Intermediate, Destination)

register_stream_sigio(Stream1, Stream2) :-
        pipe(Stream2, W),
        assert(sigio(Stream1, W, Stream2)),
        (current_thread(stream_pool_main_loop, _) ->
             delete_stream_from_pool(Stream1),  % just in case...
             add_stream_to_pool(Stream1, sigio_action(signal))
        ;
             add_stream_to_pool(Stream1, sigio_action(signal)),
             thread_create(stream_pool_main_loop, _, [detached(true)])).

unregister_stream_sigio(Stream) :-
%        sigio(Stream, _, _),  % Check stream is being watched
        %delete_stream_from_pool(Stream),
        %add_stream_to_pool(Stream, sigio_action(justcopy)),
        retract(sigio(Stream, W, _)),
        close(Stream),  % No use any more, no more data arriving to Stream
        close(W).       % Intermediate step not use anymore


register_stream_name(_, Name)      :- Name==user, !.
register_stream_name(Stream, Name) :- atom(Name), !,
				      set_stream(Stream, alias(Name)).
register_stream_name(Stream, Stream).


sigio_action(T) :-
        findall(S, sigio(S,_,_), LS),
        wait_for_input(LS, [RS|_], 0),
        (at_end_of_stream(RS) ->           % Original read stream is EOF?
             unregister_stream_sigio(RS)   % Then
        ;
             sigio(RS, W, _),          % Retrive intermediate stream W
             copy_pipe(RS, W),         % Copy from RS ----> W
             (T == signal ->
                  current_prolog_flag(pid, Pid),
                  current_signal(io, IdSignal, _),
                  kill(Pid, IdSignal)
             ;
                  true)
        ).

% Copy all current data in input-pipe-stream In to output-pipe-stream Out
copy_pipe(In, Out)   :-
        wait_for_input([In],[],0.0000000001), !, % Nothing more on In
        flush_output(Out).                       % Everything has been copied
copy_pipe(In, Out) :-
        get_char(In, CharCode),        % Get one char from stream
        (CharCode=(-1) ->
             true
        ;
             write(Out, CharCode)
        ),
        copy_pipe(In, Out).




