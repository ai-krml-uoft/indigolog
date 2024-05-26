:- module(utils, [
     % 1 - TOOLS
     % 2 - STRINGS
     join_to_string/2,
     join_to_string/3,
     any_to_string/2,
     % 3 - OS TOOLS
     set_interrupt_handler/2,
     current_interrupt/2,
     get_interrupt_handler/3,
     stime/1,
     cputime/1,
     send_term/2
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

system(ShellCommand) :- shell(ShellCommand).
sh(ShellCommand)     :- shell(ShellCommand).



send_term(Stream, Term) :-
     write_term(Stream, Term, [quoted(false), fullstop(true), nl(true)]),
     flush_output(Stream).

