%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/dev_managers.pl
%
%  AUTHOR : Sebastian Sardina (2004)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
% Definition of defaults device managers for different environments
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             July 9, 2002
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
%
% The following definition are required:
%
% -- executable_path(A, P) : P is the executable path for software A
%
% The following definition of constants are provided:
%
% -- device_manager(+S, +P, -C, [+Host, +Port]) : 
%         Retract comand C for environment P on plataform P with 
%         manager listeling at address Host/Port
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SIMULATOR DEVICE 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
device_manager(simulator, eclipse, Command, [Host, Port]):- 
        main_dir(Dir),
        concat_atom([Dir,'Env/env_sim.pl'], File),
        executable_path(xterm, Exterm),
        executable_path(eclipse, Eeclipse),
        concat_atom([Exterm, ' -e ', 
                     Eeclipse, ' -g 10M host=', Host, ' port=', Port, 
                     ' -b ', File, ' -e ', ' start'], Command).

device_manager(simulator, swi, Command, [Host, Port]):- 
        main_dir(Dir),
        concat_atom([Dir,'Env/env_sim.pl'], File),
        executable_path(xterm, Exterm),
        executable_path(swi, Eswi),
        concat_atom([Exterm, ' -e ', 
                     Eswi, ' -t ', ' start', ' -f ', File,
		     ' host=', Host, ' port=', Port,' debug=1'], Command).
		     
		     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% RCX LEGO MINDSTORM DEVICE 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
device_manager(rcx, eclipse, Command, [Host, Port]):- 
        main_dir(Dir),
        concat_atom([Dir,'Env/env_rcx.pl'], File),
        executable_path(xterm, Exterm),
        executable_path(eclipse, Eeclipse),
        concat_atom([Exterm, ' -e ', 
                     Eeclipse, ' -g 10M host=', Host, ' port=', Port, 
                     ' -b ', File, ' -e ', ' start'], Command).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INTERNET/SYSTEM DEVICE 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
device_manager(internet, eclipse, Command, [Host, Port]):- 
        main_dir(Dir),
        concat_atom([Dir,'Env/env_int.pl'], File),
        concat_atom(['xterm -e ', 
                     'eclipse -g 10M host=', Host, ' port=', Port, 
                     ' -b ', File, ' -e ', ' start'], Command).

device_manager(internet, swi, Command, [Host, Port]):- 
        main_dir(Dir),
        concat_atom([Dir,'Env/env_int_swi.pl'], File),
        executable_path(xterm, Exterm),
        executable_path(swi, Eswi),
        concat_atom([Exterm, ' -e ', 
                     Eswi, ' -t ', ' start', ' -f ', File,
		     ' host=', Host, ' port=', Port,' debug=1'], Command).

		     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ER1 DEVICE: to control the Evolution ER1 robot
%
%  ER1 CONFIGURATION FOR UOFT (DONE BY SIMON WEBER-BROWN)
%
%	1. Configure the robot laptop to lease an IP address via DHCP.
%
%	Its IP settings should be as follows:
%
%		IP: 	 172.31.0.150
%		SUBMASK: 255.255.0.0
%		GATEWAY: 172.31.0.254
%
%	2. SSH to er1.cs.toronto.edu as user <er1> (with the corresponding password)
%
%	3. To talk to the robot from outside, connect to er1.cs.toronto.edu on port 9000
%   
%	You should now be "talking" to the robot
%
% ER1 account at CS: er1 / come1er1
% ER1 account at evolution web page: er1uoft@cs.toronto.edu / come1er1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
device_manager(er1, eclipse, Command, [Host, Port]):- 
        main_dir(Dir),
        er1_location(IPER1, PORTER1),
        concat_atom([Dir,'Env/env_er1.pl'], File),
        concat_atom(['xterm -e ', 
                     'eclipse -g 10M host=', Host, ' port=', Port, 
                     ' iper1=', IPER1, ' porter1=', PORTER1, 
                     ' -b ', File, ' -e ', ' start'], Command).

% This is the address of the ER1 server
er1_location('er1.cs.toronto.edu', 9000).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% WUMPUS WORLD SIMULATOR DEVICE 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wumpus_location('127.0.0.1', 9002).

% With terminal
device_manager(virtual_wumpus, swi, Command, [Host, Port]):- 
        main_dir(Dir),
        wumpus_location(IPW, PORTW),
        wumpus_config(TIDRun,Size,PPits,NoGolds,TIDScenario),
        term_to_atom(TIDRun, IDRun),
        term_to_atom(TIDScenario, IDScenario),
        concat_atom([Dir,'Env/env_wumpus.pl'], File),
        executable_path(xterm, Exterm),
        executable_path(swi, Eswi),
        concat_atom([Exterm, ' -e ', 
                     Eswi, ' -t ', ' start', ' -f ', File,
		     ' host=', Host, ' port=', Port,' debug=1',
                     ' ipwumpus=', IPW, ' portwumpus=', PORTW,
                     ' ppits=', PPits, ' nogolds=', NoGolds, ' size=', Size,
                     ' idrun=\'', IDRun, '\' idscenario=\'', IDScenario, '\''
                     ], Command).

% Without terminal
device_manager(virtual_wumpus_silent, swi, Command, [Host, Port]):- 
        main_dir(Dir),
        wumpus_location(IPW, PORTW),
        wumpus_config(TIDRun,Size,PPits,NoGolds,TIDScenario),
        term_to_atom(TIDRun, IDRun),
        term_to_atom(TIDScenario, IDScenario),
        concat_atom([Dir,'Env/env_wumpus.pl'], File),
        executable_path(swi, Eswi),
        concat_atom([Eswi, ' -t ', ' start', ' -f ', File,
		     ' host=', Host, ' port=', Port,' debug=1',
                     ' ipwumpus=', IPW, ' portwumpus=', PORTW, 
                     ' ppits=', PPits, ' nogolds=', NoGolds, ' size=', Size, 
                     ' idrun=\'', IDRun, '\' idscenario=\'', IDScenario,'\'',
                     ' 1>/dev/null 2>/dev/null'
                     ], Command).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLIMA GAME SIMULATOR DEVICE: to communicate with the game simulator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
device_manager(clima07(LOptions), swi, (Command, LArgs), [HostEM, PortEM]):- 
        main_dir(Dir),
        clima_location(IPCLIMA, PORTCLIMA),
        clima_agentID(TAgentName, TAgentPass),
        term_to_atom(TAgentName, AgentName),
        term_to_atom(TAgentPass, AgentPass),
        concat_atom([Dir,'Env/env_clima.pl'], File),
	(member(debug(Debug), LOptions) -> true ; Debug=3),
		% Build the set of arguments
	concat_atom(['host=', HostEM], ArgHost),
	concat_atom(['port=', PortEM], ArgPort),
	concat_atom(['debug=', Debug], ArgDebug),
	concat_atom(['ipsim=', IPCLIMA], ArgIPCLIMA),
	concat_atom(['portsim=', PORTCLIMA], ArgPortCLIMA),
	concat_atom(['agentLogin=', AgentName], ArgAgentLog),
	concat_atom(['agentPass=', AgentPass], ArgAgentPass),
       	LPrologArgs=['-t','start','-f',File,ArgHost,ArgPort,ArgDebug,ArgIPCLIMA,ArgPortCLIMA,
				ArgAgentLog,ArgAgentPass],
		% Now build the final pair of (Command, LArgs)
    executable_path(swi, Eswi),
    executable_path(xterm, Exterm),
	(member(quiet, LOptions) ->
        	append(LPrologArgs,[' >/dev/null 2>/dev/null'],LPrologArgs2),
		join_atom([Eswi|LPrologArgs2], ' ',Arg),
		LArgs=['-c', Arg],
		Command=sh
	;
        	join_atom([Eswi|LPrologArgs], ' ',Arg),
		LArgs=['-e', Arg],
		Command=Exterm
	).


% This is the address of the CLIMA server
% (this would be defined in the main_xxx.pl application file)
%clima_location('localhost', 12300).
%clima_agentID(china1, 1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MESSENGER SERVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
device_manager(messenger(LOptions), swi, (Command, LArgs), [HostEM, PortEM]):- 
        main_dir(Dir),
        mess_location(IPMESS, PORTMESS),
        agentID(TAgentName),
        term_to_atom(TAgentName, AgentName),
        concat_atom([Dir,'Env/env_mess.pl'], File),
	(member(debug(Debug), LOptions) -> true ; Debug=3),
		% Build the set of arguments
	concat_atom(['host=', HostEM], ArgHost),
	concat_atom(['port=', PortEM], ArgPort),
	concat_atom(['debug=', Debug], ArgDebug),
	concat_atom(['ipmess=', IPMESS], ArgIPMESS),
	concat_atom(['portmess=', PORTMESS], ArgPortMESS),
	concat_atom(['agentLogin=', AgentName], ArgAgent),
       	LPrologArgs=['-t','start','-f',File,ArgHost,ArgPort,ArgDebug,ArgIPMESS,ArgPortMESS,ArgAgent],
		% Now build the final pair of (Command, LArgs)
    executable_path(swi, Eswi),
    executable_path(xterm, Exterm),
	(member(quiet, LOptions) ->
        	append(LPrologArgs,[' >/dev/null 2>/dev/null'],LPrologArgs2),
		join_atom([Eswi|LPrologArgs2], ' ',Arg),
		LArgs=['-c', Arg],
		Command=sh
	;
        	join_atom([Eswi|LPrologArgs], ' ',Arg),
		LArgs=['-e',Arg],
		Command=Exterm
	).


% This is the address of the MESSENGER server
% (this would be defined in the main_xxx.pl application file)
%mess_location('localhost', 3900).
%agentID(china1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Env/dev_managers.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
