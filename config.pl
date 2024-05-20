/*
	IndiGolog configuration file

	This file contains various wide-system configuration variables and options
	such as location of files/modules, libraries, constants, etc.

	@author ssardina - 2002-2024
*/

% asserts root_indigolog/1 with the path to the IndiGolog root folder
:- prolog_load_context(directory, Dir), assert(root_indigolog(Dir)).

main_dir(Path):- root_indigolog(Path).


% This is the initialization needed for each type of Prolog used
:- 	root_indigolog(Dir),
   	directory_file_path(Dir, 'lib', LibDir),
   	assert(library_directory(LibDir)),
	use_module(library(eclipse_swi)), init_eclipse_lib, % ECLIPSE Compat lib
	use_module(library(tools_swi)),
	use_module(library(time)),	% for call_with_time_limit/2
	style_check(-discontiguous),
	set_prolog_flag(optimise, true),
	set_backquoted_string.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% location of various modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dir(indigolog_plain, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "interpreters/indigolog_plain_swi.pl", F).
dir(indigolog, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "interpreters/indigolog.pl", F).

dir(env_manager, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "env/env_man.pl", F).
dir(eval_bat, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "eval/eval_bat.pl", F).


dir(dev_managers, F) :-
	root_indigolog(Dir),
	directory_file_path(Dir, "env/dev_managers", F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Defines the path of executables used to define device managers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
executable_path(swi, '/usr/bin/swipl').
executable_path(eclipse, '/opt/bin/eclipse-pl').  % if available
executable_path(tcltk, '/usr/bin/wish').
executable_path(xterm, '/usr/bin/xterm').



