%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Elevator-BAT/main_swi.pl
%
%  AUTHOR : Sebastian Sardina (2002)
%	    based on the original code written by Hector Levesque
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%
%  This is example3 for the first IndiGolog code written by H. Levesque
%  It is the elevator that appears in the IJCAI-97 paper on ConGolog
%  It uses exogenous actions for temperature, smoke, and call buttons
%
%
% Written for SWI Prolog http://www.swi-prolog.org/) running under Linux
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             June 15, 2000
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
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
%
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
% This is the top-level file for a Legolog application program.
% It consults all the necessary Legolog prolog files.
% In particular, the following is loaded:
%
%  (1) Load all libraries required. This includes the system dependant
%      ones for the specific Prolog plus general libraries
%  (2) Load the IndiGolog interpreter and the projector used
%  (3) Load the application code itself containing the background theory
%      of action plus the high-level program
%  (4) Specify which environments should be loaded and how
%  (5) Specify how each action should be executed and how to translate
%      exogenous actions
%
% Moreover, the following is provided:
%
% -- main: Collects all the procedures named 'mainControl(id)'
%	   and asks the user which one to run. Uses controller/1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic controller/1.	% Stores the user decision on the controller to run


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- reset_backquoted_string.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consult the top-level interpreter, environent manager and projector
:- dir(indigolog, F), consult(F).
:- dir(env_man, F), consult(F).
:- dir(eval_bat, F), consult(F).

% Consult application
:- [elevator].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Any port available would be ok for the EM.
server_port(_).
%server_host('localhost').  % this is the default anyways...



% Load simulator, RCX and internet environments
:- dir(dev_managers, F), consult(F).
load_dev(simulator, swi).

load_device(Env, Command, Address) :-
	findall((Env, Type), load_dev(Name, Type), Dev),
        member((Env,Type), Dev),
        (var(Address) -> (Host = null, Port = null) ; Address = [Host, Port]),
        device_manager(Env, Type, Command, [Host, Port]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code
%        how_to_execute(Action, Environment, Code)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_to_execute(Action, simulator, Action).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION
%          translateExogAction(Code, Action)
%          translateSensing(Action, Outcome, Value)
% OBS: If not present, then the translation is 1-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(CodeAction, Action) :- actionNum(Action, CodeAction).
translateSensing(_, SensorValue, SensorValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets IndiGolog to evaluate a chosen mainControl procedure
main :-
    findall(C, proc(control(C), _), L),
    repeat,
    format('Controllers available: ~w\n', [L]),
    write('Select controller: '),
	read(Ctrl), nl,
    member(S, L),
	format('Executing controller: *~w*\n', [S]), !,
    main(Ctrl).

main(C) :- assert(controller(C)), indigolog.


:- set_option(debug_level,0).
:- set_option(wait_step,3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Elevator-BAT/main_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%