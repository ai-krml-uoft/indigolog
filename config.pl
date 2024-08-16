/* 	INDIGOLOG configuration file

	This file contains various wide-system configuration variables and options
	such as location of files/modules, libraries, constants, etc.

	@author ssardina 2002-2024 - ssardina@cs.toronto.edu, ssardina@gmail.com
*/

% asserts root_indigolog/1 with the path to the INDIGOLOG root folder
:- prolog_load_context(directory, Dir), assert(root_indigolog(Dir)).

main_dir(Path):- root_indigolog(Path).

:- ['lib/common.pl'].

:- 	root_indigolog(Dir),
   	directory_file_path(Dir, 'lib', LibDir),
   	assert(library_directory(LibDir)),
	use_module(library(utils)),
	use_module(library(time)),	% for call_with_time_limit/2
	% style_check(-discontiguous),	% use it where wanted
	set_prolog_flag(optimise, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% location of various modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dir(indigolog_plain, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "interpreters/indigolog_plain.pl", F).
dir(indigolog, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "interpreters/indigolog.pl", F).

dir(env_manager, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "interpreters/env_man.pl", F).

dir(dev_simulator, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "devices/dev_sim.pl", F).

dir(eval_bat, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "eval/eval_bat.pl", F).

dir(exog_tcltk_, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "devices/exog.tcl", F).

%  this consults the standard indigolog interpreter
% :- dir(indigolog, F), consult(F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Defines the path of executables used to define device managers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
executable_path(swi, '/usr/bin/swipl').
executable_path(eclipse, '/opt/bin/eclipse-pl').  % if available
executable_path(tcltk, '/usr/bin/wish').
executable_path(xterm, '/usr/bin/xterm').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


