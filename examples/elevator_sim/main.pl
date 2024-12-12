/*  Elevator Simulator Application MAIN file

    @author Sebastian Sardina - ssardina@gmail.com

    This file is the main file for the elevator simulator application. It
    loads the necessary files and starts the application.

    The application is a simple elevator simulator that is controlled by
    an INDIGOLOG program. A TCL/TK interface can be used to issue
    exogenous events/actions.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT INDIGOLOG FRAMEWORK
%
%    Configuration files
%    Interpreter
%    Environment manager
%    Evaluation engine/Projector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- ['../../config.pl'].   % now loaded when calling swipl

:- dir(indigolog, F), consult(F).
:- dir(eval_bat, F), consult(F).    % after interpreter always!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT APPLICATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [elevator].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPECIFY ADDRESS OF ENVIRONMENT MANAGER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Any port available would be ok for the EM.
em_address(localhost, 8000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ENVIRONMENTS/DEVICES TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_devices([simulator]).

% start env_sim.pl tcl/tk interaction interface

load_device(simulator, Host:Port, [pid(PID)]) :-
	dir(dev_simulator, File),
	ARGS = ['-e', 'swipl', '-t', 'start', File, '--host', Host, '--port', Port],
	logging(
		info(5, app), "Command to initialize device simulator: xterm -e ~w", [ARGS]),
	process_create(
		path(xterm), ARGS,
		[process(PID)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code
%        how_to_execute(Action, Environment, Code)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

how_to_execute(Action, simulator, sense(Action)) :-
	sensing_action(Action, _).

how_to_execute(Action, simulator, Action) :-
	 \+ sensing_action(Action, _).


fixed_events(["heat", "cold", "smoke", "reset"]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION
%
%          translate_exog(Code, Action)
%          translate_sensing(Action, Outcome, Value)
%
% OBS: If not present, then the translation is 1-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_exog(ActionCode, Action) :-
	actionNum(Action, ActionCode), !.
translate_exog(A, A).
translate_sensing(_, SR, SR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets INDIGOLOG to evaluate a chosen mainControl procedure

main :-
	findall(C,
		proc(
			control(C), _), LC),
	length(LC, N), repeat,
	format('Controllers available: ~w\n', [LC]),
	forall(
		(
			between(1, N, I),
			nth1(I, LC, C)),
		format('~d. ~w\n', [I, C])), nl, nl,
	write('Select controller: '),
	read(NC), nl,
	number(NC),
	nth1(NC, LC, C),
	format('Executing controller: *~w*\n', [C]), !,
	main(
		control(C)).

main(C) :-
	assert(
		control(C)),
	indigolog(C).


:- set_option(log_level, 5).
:- set_option(log_level, em(1)).
:- set_option(wait_step, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%