%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: CLIMA/main_swi.pl
%
%  AUTHOR	: Sebastian Sardina
%  email	: ssardina@cs.toronto.edu
%  WWW		: www.cs.toronto.edu/cogrobo
%  TYPE CODE	: system dependent predicates
%  TESTED	: SWI Prolog 5.6.24 under FC6 
%
% IndiGolog agent player for CLIMA-07
%
% Written for SWI Prolog http://www.swi-prolog.org/) running under Linux
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% -- main: Collects all the procedures named 'mainControl(N)' where
%          N is the number representing the N-th controller.
%          The user can select which controller to execute and the 
%          IndiGolog executor will be run on such controller
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET GLOBAL PARAMETERS AND GLOBAL VARIABLES/CONSTANTS USED
%  
%  These may be options to improve performance and variables/constants used
%  around the whole arquitecture
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) LOAD/COMPILE/IMPORT LIBRARIES, MODULES, ETC that may be required.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include('../../lib/systemvar').  % Global include code and Prolog init
:- consult('../../lib/alpha_star'). % Alpha* path finding
%:- use_module(library(chr)).
%:- reset_backquoted_string.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (2,3) CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1 - Consult the IndiGolog system: top-level and evaluator
:- consult('../../Interpreters/indigolog').     % IndiGolog interpreter 
% :- consult('../../Eval/eval_know').               % LP evaluator
:- consult('../../Eval/evalbat').               % LP evaluator

% 2 - Consult environment manager 
:- consult(['../../Env/env_man.pl']).         % Load environment manager

% 3 - Consult application
:- consult(agent_bat).                          % Application code in IndiGolog

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (4,5) ENVIRONMENTS TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic 
	clima_agentID/2, 
	teammember/1, 
	mess_location/2,
	clima_location/2.


% This is the address of the CLIMA GAME server environment
%clima_location('tea.dyndns.org', 12300).
clima_location(teahp, 12300).
%clima_location(localhost, 12300).
%clima_location('agentslave.in.tu-clausthal.de', 12300).
clima_agentID(participant1,1).	% default


% set the agent ID and PASSWORD and the corresponding teammates
set_agentID(AgentId,PassId) :-
	retractall(clima_agentID(_,_)),
	assert(clima_agentID(AgentId, PassId)).


% This is the address and information for the MESSENGER environment
%mess_location('tea.dyndns.org', 12340).
mess_location(teahp, 12340).
%mess_location(localhost, 12340).
agentID(Id) :- clima_agentID(Id,_).
teammember(participant1).	% Default team-members
teammember(participant2).
teammember(participant3).
teammember(participant4).
teammember(participant5).
teammember(participant6).

% set the team
set_team(ListPlayers) :- 
	retractall(teammember(_)), 
	member(Player, ListPlayers),
	assert(teammember(Player)),
	fail.
set_team(_).


% Port of environment manager has to be fixed in SWI
server_port(_).
%server_host('127.0.0.1').
server_host(localhost).


% Define what environment managers the application will use
:- ['../../Env/dev_managers'].              % Common facts (device_manager/4)
load_device(Env, Command, Address) :- 
        member((Env,Type), [(clima07([quiet]), swi),(messenger([quiet]), swi)]),
        %member((Env,Type), [(clima07([debug(5)]), swi),(messenger([]), swi)]),
        (var(Address) -> 
             Host=null, Port=null ; 
             Address = [Host, Port]
        ),
        device_manager(Env, Type, Command, [Host, Port]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code
%        how_to_execute(Action, Environment, Code)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_to_execute(Action, messenger(_), Action) :-
	member(Action, [tell(_,_), broadcast(_)]), !.
how_to_execute(Action, clima07(_), Action) :-
	clima_action(Action).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION
%          translateExogAction(Code, Action)
%          translateSensing(Action, Outcome, Value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translateExogAction(CodeAction, Action) :- 
	actionNum(Action, CodeAction).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% participant team
main1:- set_participant_team, main(participant1,1).
main2:- set_participant_team, main(participant2,2).
main3:- set_participant_team, main(participant3,3).
main4:- set_participant_team, main(participant4,4).
main5:- set_participant_team, main(participant5,5).
main6:- set_participant_team, main(participant6,6).
set_participant_team :- 
	set_team([participant1,participant2,participant3,participant4,participant5,participant6]).


% golog team
golog1 :- set_golog_team, main('GOLOGteam1',va5Liove).
golog2 :- set_golog_team, main('GOLOGteam2','Aerai6Pa').
golog3 :- set_golog_team, main('GOLOGteam3','Efool1lu').
golog4 :- set_golog_team, main('GOLOGteam4',cahk6Oi7).
golog5 :- set_golog_team, main('GOLOGteam5',iuY1soj5).
golog6 :- set_golog_team, main('GOLOGteam6',deiZak5f).
set_golog_team :- 
	set_team(['GOLOGteam1','GOLOGteam2','GOLOGteam3','GOLOGteam4','GOLOGteam5','GOLOGteam6']).


% set an agent player and go!
main(AgentId, PassId) :-
	set_agentID(AgentId,PassId), !,
	indigolog.	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_option(debug_level,0).
:- set_option(wait_step,0).
:- set_option(debug_level,warn_off).



run_firefox :-
        (    fork(child),
             exec(xterm)
        ;    true
        ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: CLIMA/main_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

requestAction(1177840673414, [step(87), posX(26), posY(26), items(1), deadline(1177840677414), id('88'), cells([cell(cur, [agent(ally)]), cell(n, [obstacle]), cell(nw, [obstacle]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])])




and(report('Checking for gold around us...'), and(neg(fullLoaded), and(apply(dir, [locRobot(me), loc]), and(isGold(loc)=true, report('Spotted gold around! Moving there...')))))






*/