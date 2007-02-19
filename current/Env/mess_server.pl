%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE     : Env/mess_server.pl
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system dependent code
%  TESTED : SWI Prolog 5.0.10 www.swi-prolog.org
%
% An environment to communicate among multiple agents
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             November 15, 2002
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
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- include('../lib/systemvar'). % Global include code and Prolog init
%:- use_module(library(streampool)). % required bu alreaedy in systemvar

% define how many agent connections will be allowed
max_no_agents(10).

server(Port) :-
	report_message(system(0), ['Starting MESSENGER server at port ', Port]),
	close_socket(Port),
	retractall(agent(_,_,_,_)),
	%	
        tcp_socket(Socket),
        tcp_bind(Socket, Port),
	max_no_agents(NoAgents),	% how many agents can connect?
        tcp_listen(Socket, NoAgents),
        tcp_open_socket(Socket, In, _Out),
        add_stream_to_pool(In, accept_new_agent(Socket)),
        stream_pool_main_loop.

accept_new_agent(Socket) :-
        tcp_accept(Socket, Slave, PeerIP),
        tcp_open_socket(Slave, In, Out),
	report_message(system(0), ['Adding a new agent from IP: ', PeerIP]),
	assert(agent(noname,In,Out,PeerIP)),	% register connection of agent
        add_stream_to_pool(In, handle_client(In)).


handle_client(In) :-
	read_term(In, Message, []),  
	agent(Agent,In,_,_),
	report_message(system(3), ['Message from ',(Agent,In),' : ',Message]),
        handle_message(In, Message).



handle_message(In, unregister) :-
	unregister_agent(In).

handle_message(In, register(Agent)) :-
	retract(agent(_,In,Out,PeerIP)),
	assert(agent(Agent,In,Out,PeerIP)),
	tell(server,Agent,ok),
	report_message(system(2), ['Agent ',Agent,' registered']).



handle_message(In, tell(AgentRcv,Message)) :-
	agent(AgentSrc,In,_,_),
	AgentSrc\=noname,
	tell(AgentSrc, AgentRcv, Message).

handle_message(In, broadcast(Message)) :-
	agent(AgentSrc,In,_,_),
	AgentSrc\=noname,
	findall(AgentRcv,agent(AgentRcv,_,_,_),AgentRcv\=AgentSrc, LAgents),
	tell_all(AgentSrc,LAgents,Message).

handle_message(In,_) :-
	at_end_of_stream(In),
	unregister_agent(In).
	

handle_message(In,Mess) :- 
	write('******* Message cannot be handled: '), 
	write(In), write('-'), writeln(Mess).




% TOOLS
unregister_agent(In) :-
	retract(agent(Agent,In,Out,_PeerIP)),
        delete_stream_from_pool(In),
	close_socket(In),
        close_socket(Out),
	report_message(system(2), ['Agent ',Agent,' unregistered']).


tell(AgentSrc, AgentRcv, Message) :-
	agent(AgentRcv,_,Out,_),
        write_term(Out, told(AgentSrc,Message), [quoted(true)]),
        write(Out, '.'),
        nl(Out),
        flush_output(Out).

tell_all(_,[],_).
tell_all(AgentSrc,[AgentRcv|LAgents],Message) :-
	tell(AgentSrc, AgentRcv, Message),
	tell_all(AgentSrc,LAgents,Message).



% Tries to close socket X but catches the exception if not possible
close_socket(X) :-
	catch(close(X),E,report_message(warning,['Cannot close socket ',X,'--> ',E])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF:  Env/mess_server.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%