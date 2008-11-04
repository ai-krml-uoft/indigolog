%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: PMS/pms.pl
%
%  AUTHOR : Andrea Marrella and Stefano Valentini
%  EMAIL  : marrella@dis.uniroma1.it, and182@iol.it, a.marrella@gmail.com
%	    stefano_valentini82@libero.it
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.id_.1id_ http://www.swi-prolog.org
%
%  This file contains 2 controllers
%
%  mainControl(1) : (example2.pl in the original IndiGolog)
%  The dumb controller tries without search but commits too soon       
%
%  mainControl(3) : (example3.pl in the original IndiGolog)
%  This is the elevator that appears in the IJCAI-97 paper on ConGolog 
%  It uses exogenous actions for temperature, smoke, and call buttons  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             Feb 22, 2
%
% The indigolog software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
% 
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
% 
%         Copyright (c) by The University of Toronto,
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
%  A basic action theory (BAT) is described with:
%
% -- fun_fluent(fluent)     : for each functional fluent (non-ground)
% -- rel_fluent(fluent)     : for each relational fluent (non-ground)
%
%           e.g., rel_fluent(painted(C)).
%           e.g., fun_fluent(color(C)).
%
% -- prim_action(action)    : for each primitive action (ground)
% -- exog_action(action)    : for each exogenous action (ground)
%
%           e.g., prim_action(clean(C)) :- domain(C,country).
%           e.g., exog_action(painte(C,B)):- domain(C,country), domain(B,color).
%
% -- senses(action,fluent)  : for each sensing action
%
%           e.g, poss(check_painted(C),  painted(C)).
%
% -- poss(action,cond)      : when cond, action is executable
%
%           e.g, poss(clean(C),   and(painted(C),holding(cleanear))).
%
% -- initially(fluent,value): fluent has value in Sid_ (ground)
%
%          e.g., initially(painted(C), false):- domain(C,country), C\=3.
%                initially(painted(3), true).
%                initially(color(3), blue).
%
% -- causes_val(action,fluent,value,cond)
%          when cond holds, doing act causes functional fluent to have value
%
%            e.g., causes_val(paint(C2,V), color(C), V, C = C2).
%               or causes_val(paint(C,V), color(C), V, true).
%
% -- causes_true(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to hold
% -- causes_false(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to not hold
%
%            e.g., causes_true(paint(C2,_), painted(C), C = C2).
%               or causes_true(paint(C,_), painted(C), true).
%            e.g., causes_false(clean(C2),  painted(C), C = C2).
%               or causes_false(clean(C),  painted(C), true).
%
% -- sort(name,domain_of_sort).      : all sorts used in the domain
%
%        e.g., varsort(c, colors).
%              varsort(temp, temperature).
%              color([blue, green, yellow, red]).       
%              temperature([-1id_,id_,1id_,2id_,3id_,4id_]).
%
%
% A high-level program-controller is described with:
%
% -- proc(name,P): for each procedure P 
% -- simulator(N,P): P is the N exogenous action simulator
%
% The interface for Lego is described with:
%
% -- actionNum(action, num)  
%         action has RCX code num
% -- simulateSensing(action)
%         sensing result for action should be asked to the user
% -- translateSensing(action, sensorValue, sensorResult) 
%         translate the sensorValue of action to sensorResult
% -- translateExogAction(codeAction, action) 
%         translateSensing action name into codeAction and vice-versa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic controller/1.


/* DOMAINS-SORTS AVAILABLE */

/* Domain of possible services */
operators([1,2,3,4,5,6,7]).
/* Domain of possible tasks executables by services */
tasks([photo,rescue,evacuation,survey,senddata,evaluatephoto,census]).
/* Domain representing the capabilities */
capabilities([makephoto,compilesurvey,compilecensus,gprs,evaluation,rescues]).

identifiers([id_1,id_2,id_3,id_4,id_5,id_6,id_7,id_8,id_9,id_10,id_11,id_12,id_13,id_14,id_15,id_16,id_17,id_18,id_19,id_20,id_21,id_22,id_23,id_24,id_25,id_26,id_27,id_28,id_29,id_30,id_31,id_32,id_33,id_34,id_35,
id_36,id_37,id_38,id_39,id_40,id_41,id_42,id_42,id_44,id_45,id_46,id_47,id_48,id_49,id_50]).

/* There is nothing to do caching on (required becase cache 1 is static)*/
cache(_):-fail.

service(N) :- domain(N,operators).
task(X) :- domain(X,tasks).
capability(B) :- domain(B,capabilities).
id(D) :- domain(D,identifiers).

/*  FLUENTS and CAUSAL LAWS */

causes_true(_,_,_) :- false.
causes_false(_,_,_) :- false.

rel_fluent(provide(N,B)) :- service(N), capability(B).

rel_fluent(required(X,B)) :- task(X), capability(B).

rel_fluent(started(X,D,N)) :- task(X), service(N), id(D).
causes_val(end(X,D,N,O,V),started(X,D,N),false,started(X,D,N)=true).
causes_val(begin(X,D,N),started(X,D,N),true,and(assigned(X,D,N)=true,available(N)=true)).

rel_fluent(assigned(X,D,N)) :- task(X),service(N),id(D).           % assigned(X,D,N) : indicate that task X with id D has been assigned to service N
causes_val(assign(X,D,N,I,O),assigned(X,D,N),true,true).        
causes_val(release(X,D,N),assigned(X,D,N),false,true).      

rel_fluent(available(N)) :- service(N).           			  % Il servizio N è disponibile ad eseguire il servizio X.
causes_val(end(X,D,N,O,V),available(N),true,started(X,D,N)=true).         % available(N,X) è vero quando il servizio N rilascia il task X
causes_val(begin(X,D,N),available(N),false,assigned(X,D,N)=true).         % available(N,X) è falso quando il PMS assegna al servizio N il task X

rel_fluent(kind_assigned(X,N)) :- task(X),service(N).           % assigned(X,D,N) : indicate that task X with id D has been assigned to service N
causes_val(assign(X,D,N,I,O),kind_assigned(X,N),true,true).        
causes_val(release(X,D,N),kind_assigned(X,N),false,true).      

/*  FLUENTS GENERATED FOR TASKS OUTPUT */

fun_fluent(numphoto(D)) :- id(D).
causes_val(end(X,D,N,O,V),numphoto(D),V,O=numphoto).

fun_fluent(evaluate(D)) :- id(D).
causes_val(end(X,D,N,O,V),evaluate(D),V,O=evaluate).


  /*  ACTIONS and PRECONDITIONS*/

prim_action(assign(X,D,N,I,O)) :- task(X), service(N), id(D).       % Assign task X identified by D to service N, with input I and output O  
poss(assign(X,D,N,I,O), true).

prim_action(stop(X,D,N)) :- task(X), service(N), id(D).
poss(stop(X,D,N), true).

prim_action(start(X,D,N)) :- task(X), service(N), id(D).
poss(start(X,D,N), true).

prim_action(release(X,D,N)) :- task(X), service(N), id(D).
poss(release(X,D,N), true).

prim_action(waitStarting(X,D,N)) :- task(X), service(N), id(D).
poss(waitStarting(X,D,N), true).

prim_action(waitEnding(X,D,N,O)) :- task(X), service(N), id(D).
poss(waitEnding(X,D,N,O), true).

prim_action(loading(X,D)) :- task(X), id(D).
poss(loading(X,D), true).

	 /* EXOGENOUS ACTIONS*/

exog_action(begin(X,D,N)) :- task(X), service(N), id(D).
exog_action(end(X,D,N,O,V)) :- task(X), service(N), id(D).

/* ABBREVIATIONS */

proc(isAvailable(N), available(N)=true).
proc(isStarted(X,D,N), started(X,D,N)=true).
proc(isProvided(N,B), provide(N,B)=true).
proc(isRequired(X,B), required(X,B)=true).

/* INITIAL STATE:  */
initially(available(N),true) :- service(N).

initially(assigned(X,D,N),false) :- task(X), service(N), id(D).
initially(kind_assigned(X,N),false) :- task(X), service(N).

initially(started(X,D,N),false) :- task(X), service(N), id(D).

initially(provide(N,B),false) :- service(N), capability(B), N\=1, N\=2, N\=3, N\=4, N\=5, N\=6, N\=7.

initially(provide(1,gprs),true).
initially(provide(1,evaluation),true).

initially(provide(2,compilecensus),true).
initially(provide(2,rescues),true).

initially(provide(3,compilecensus),true).
initially(provide(3,rescues),true).

initially(provide(4,compilecensus),true).
initially(provide(4,rescues),true).

initially(provide(5,compilesurvey),true).
initially(provide(5,makephoto),true).

initially(provide(6,compilesurvey),true).
initially(provide(6,makephoto),true).

initially(provide(7,compilesurvey),true).
initially(provide(7,makephoto),true).

initially(required(X,B),false) :- task(X), capability(B), X\=photo, X\=rescue, X\=evacuation, X\=survey, X\=senddata, X\=evaluatephoto, X\=census.

initially(required(photo,makephoto),true).
initially(required(survey,compilesurvey),true).
initially(required(census,compilecensus),true).
initially(required(senddata,gprs),true).
initially(required(evaluatephoto,evaluation),true).
initially(required(rescue,rescues),true).
initially(required(evacuation,rescues),true).

initially(numphoto(D),0) :- id(D).
initially(evaluate(D),false) :- id(D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN PROCEDURE FOR INDIGOLOG

proc(main,  mainControl(N)) :- controller(N), !.
proc(main,  mainControl(3)). % default one

proc(manageTaskAssignation(X,D,I,O), pi(b,[?(isRequired(X,b)),pi(n,[?(and(kind_assigned(X,n)=false,and(isProvided(n,b),isAvailable(n)))), assign(X,D,n,I,O), while(neg(isStarted(X,D,n)),waitStarting(X,D,n)), start(X,D,n), while(isStarted(X,D,n),waitEnding(X,D,n,O)),stop(X,D,n),release(X,D,n)])])).

proc(mainControl(5), [
itconc([
 [itconc([
         [manageTaskAssignation(rescue,id_8,location,res)],
         [manageTaskAssignation(rescue,id_9,location,res)],
	 [manageTaskAssignation(rescue,id_10,location,res)]
        ]),
  itconc([
         [manageTaskAssignation(evacuation,id_11,location,evac),manageTaskAssignation(census,id_12,location,cens)],
         [manageTaskAssignation(evacuation,id_15,location,evac),manageTaskAssignation(census,id_16,location,cens)],
         [manageTaskAssignation(evacuation,id_13,location,evac),manageTaskAssignation(census,id_14,location,cens)]
        ])
 ],
 [
  while(or(numphoto(id_2)+numphoto(id_3)+numphoto(id_4)<20,evaluate(id_7)=false),
  [itconc([
         [manageTaskAssignation(photo,id_2,location,numphoto),manageTaskAssignation(survey,id_5,location,questionnaire)],
         [manageTaskAssignation(photo,id_4,location,numphoto),manageTaskAssignation(survey,id_18,location,questionnaire)],
         [manageTaskAssignation(photo,id_3,location,numphoto),manageTaskAssignation(survey,id_6,location,questionnaire)]
         ]),
          manageTaskAssignation(evaluatephoto,id_7,location,evaluate)])
 ]
]),                           
manageTaskAssignation(senddata,id_17,information,sendingok)
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: ElevatorSim-BAT/elevator.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
