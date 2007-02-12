%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE      	: readme.txt
%  DATE	     	: January 2007
%  AUTHOR 	: Sebastian Sardina 
%  EMAIL  	: ssardina@cs.toronto.edu
%  WWW    	: www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  DESCRIPTION	: top-level readme.txt file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is the root of the IndiGolog system. There are a few things you should
know before running IndiGolog:

(1) The files conforming the whole implementation are scattered among many
different directories. Knowing the logical structure facilitates the task of 
finding files and locating specific code depending on its nature. From the 
architecture initial directory, the logical directory structure is as follows:

Doc/ 		Documentation & manuals.
Env/ 		All code related to the handling of the external environments.
		(e.g., simulation env., LEGO environment, ER1 environment, etc.)
Eval/		Temporal projectors or evaluation procedures
Interpreters/ 	Main interpreter files and any transition system available.
lib/		Compatibility and tool libraries, global definitions.
Examples/	Domain applications (e.g., elevator controller, Wumpus World)


(2) Most of the code was mostly designed and tested with SWI Prolog 
(http://www.swi-prolog.org/). Nevertheless, the whole architecture should also 
run with ECLIPSE Prolog (http://eclipse.crosscoreop.com/) which sometimes is
needed when the application uses constraint programming.

(3) Each application has its own sub-directory inside the Examples top-level
directory. By convention, the application is loaded by consulting a file
with a name of the form "main_xxx.pl" where xxx denotes the specific Prolog
platform. For example, main_swi.pl is the file to load if using SWI Prolog and
main_ecl.pl is the file to load if using ECLIPSE Prolog instead. 


(4) Before you run any application you have to make sure that:

	- The environment variable PATH_INDIGOLOG points to the architecture 
	root directory, that is, the one where this readme.txt file is. 
	- The environment variable ECLIPSELIBRARYPATH should point to
	$PATH_INDIGOLOG/lib. This is where ECLIPSE will look for user libraries.
	- SWI Prolog should be accessible with "pl" executable and ECLIPSE
	Prolog should be accessible with "eclipse" executable. If your Prolog
	interpreters have other executable, please create symbolic links called
	"pl" and "eclipse", respectively.
	- SWI Prolog should be compiled with the CLIB library
	 (http://www.swi-prolog.org/packages/clib.html). For example, in Ubuntu
	make sure that package swi-prolog-clib is installed. The CLIB library
	provides support for multi-threading and TCP/IP facilities in SWI.



(5) There are three examples that work as simulations. You may want to run these
examples before running IndiGolog in a real platform (e.g., a real robot, the
Internet, etc.). 

	- Examples/Elevator-Vanilla. This is the simplest application and it
	uses the vanilla IndiGolog interpreter (indigolog-vanilla.pl). Thus, it
	does not require any advance Prolog capability (like TCP/IP or threads).
	Everything is run in the same terminal window. You can run the five
	examples by going into the directory, loading Prolog, consulting file
	mainN_swi, where N is the example you want to run, and posting the goal
	"main". In principle, this should work with any Prolog implementation,
	not just SWI-Prolog.

	- Examples/ElevatorSim-BAT. This application is basically the same examples
	as the vanilla elevator example but it uses the non-vanilla implementation
	of IndiGolog. You should first try this example to familiarize with how
	the architecture runs. It still runs in a simulated world, but not just 
	everything in the same terminal. There is a special simulation environment 
	implementation that will open a new terminal and a TCL/TK interface where
	the user can enter exogenous actions in an asynchronous way.

	- Examples/ElevatorWumpus-KBAT. This is the Wumpus World implementation.
	It still uses a simulated world and the results are displayed in a Java
	window.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: readme.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
