# IndiGolog: A High-Level Programming Language for Embedded Reasoning Agents

This repo contains the **IndiGolog** interpreter and other pieces of code to enable IndiGolog progarms to run in external environments (e.g., Lego Mindstorm robotic platforms). 

_What is IndiGolog?_ **IndiGolog** is a high-level programming language for autonomous agents that sense their environment and do planning as they operate. Instead of classical planning, it supports _high-level program execution_. The programmer provides a _high-level nondeterministic program_ involving domain-specific actions and tests to perform the agent's tasks. The IndiGolog interpreter then reasons about the preconditions and effects of the actions in the program to find a legal terminating execution. To support this, the programmer provides a declarative specification of the domain (i.e., primitive actions, preconditions and effects, what is known about the initial state) in the Situation calculus. The programmer can control the amount of non-determinism in the program and how much of it is searched over. The language is rich and supports concurrent programming. Programs are executed online together with sensing the environment and monitoring for events, thus supporting
the development of reactive agents.

A report on the language and interpreter can be found in the following article:

* Giuseppe De Giacomo, Yves Lespérance, Hector J. Levesque, Sebastian Sardiña: [IndiGolog: A High-Level Programming Language for Embedded Reasoning Agents](https://dblp.uni-trier.de/pid/g/GDGiacomo.html). Multi-Agent Programming, Languages, Tools and Applications 2009: 31-72.


## A bit of history...

High-level programming for cognitive agents under a situation calculus theory of action stated with **Golog** in the late 90s, as reported in:

* Hector J. Levesque, Raymond Reiter, Yves Lespérance, Fangzhen Lin, Richard B. Scherl: [GOLOG: A Logic Programming Language for Dynamic Domains](https://www.sciencedirect.com/science/article/pii/S0743106696001215?via%3Dihub). Journal of Logic Programming 31(1-3): 59-83 (1997)

Later, **ConGolog** extended the language to include various _concurrency_ constructs:

Giuseppe De Giacomo, Yves Lespérance, Hector J. Levesque: [ConGolog, a concurrent programming language based on the situation calculus](https://linkinghub.elsevier.com/retrieve/pii/S000437020000031X). Artificial Intelligence 121(1-2): 109-169 (2000)

However, both systems (and some variants) were meant to solve programs completely _offline_: a legal terminating execution of the program would be extracted before executing even the first action in the domain. To better deal with dynamic systems, **IndiGolog** proposed to execute programs _online_, or _incrementally_, by default, that is, obtain just the next legal action, perform it in the world, sense the environment, and continue. When lookahead reasoning is required, a special _search_ construct is provided that allows to completely solve---find a full execution---a local sub-program offline. The most relevant papers on IndiGolog are:

* Giuseppe De Giacomo, Hector J. [An incremental interpreter for high-level programs with sensing](https://doi.org/10.1007/978-3-642-60211-5_8). In Logical Foundation for Cognitive Agents: Contributions in Honor of Ray Reiter, H. J. Levesque and F. Pirri, Eds. Springer, Berlin, 86–102 (1999).
* Giuseppe De Giacomo, Hector J. Levesque, Sebastian Sardiña: [Incremental execution of guarded theories](https://doi.org/10.1145/383779.383782). ACM Trans. Comput. Log. 2(4): 495-525 (2001)
* Giuseppe De Giacomo, Yves Lespérance, Hector J. Levesque, Sebastian Sardiña: [IndiGolog: A High-Level Programming Language for Embedded Reasoning Agents](https://dblp.uni-trier.de/pid/g/GDGiacomo.html). Multi-Agent Programming, Languages, Tools and Applications 2009: 31-72

## Pre-requisites & setup

The IndiGolog engine and action theory reasoner is all written in Prolog.

Most of the code was designed and tested with the [SWI-Prolog](http://www.swi-prolog.org/). Nevertheless, the whole architecture should also run with [ECLiPSe](http://eclipse.https://eclipseclp.org/), which is more tailored towards constraint programming.

To run the IndiGolog architecture:

1. Change the file `lib/systemvar.pl` to set your particular paths to some executables that may be used, such as SWI-Prolog, ECLiPSe, xterm, etc. For example, SWI-Prolog executable is assumed to be `/usr/bin/pl`
 but it can be changed by modifying the `executable_path/2` predicate in `lib/systemvar.pl` file.

2. TCP/IP is used for most applications (though not the Elevator-Vanilla).
   For SWI Prolog, make sure it is compiled with TCP/IP support. (This means
   that the library CLIB must be installed).

3. Extract indigolog-linux-vvv.tar.gz to some path <mypath> 
       cd <mypath>
       tar -xzf indigolog-linux-vvv.tar.gz
   where vvv is the version of Indigolog being installed. 

4. Set an environment variable PATH_INDIGOLOG to <mypath>/indigolog-linux-vvv
   For example, if <mypath> is /home/ssardina/code/ then in bash you do this:
       export PATH_INDIGOLOG=/home/ssardina/code/indigolog-linux-vvv

5. Set the right options for predicate executable_path/2 in file lib/systemvar.pl
so that IndiGolog knows where certain programs are (e.g., xterm)



For some applications, you will also need to do the following:

1. Install Tcl/Tk and the 'wish' executable.  (This is used by the simulator
   environment to enter exogenous actions.)

2. Install a Java Runtime Environment (JRE) on your system.  Make sure that
   'java' invokes the Java application launcher.  Also, note that JRE1.5 from
   SUN is known to work, while the GNU Java Compiler (GJC) is reported to fail
   when executing the script at step 5.  (This is used in the Wumpus example.)

3. If the application uses ECLIPSE Prolog (e.g., Wumpus-FLUX), the environment
   variable ECLIPSELIBRARYPATH should be set to $PATH_INDIGOLOG/lib.
   This is where ECLIPSE will look for user libraries.



## Dir structure


- `Doc/`: Documentation and manuals.
- `Env/`: Code forhandling of external environments (e.g., simulation, LEGO environment, ER1 environment, etc.).
- `Eval/`: Temporal projectors or evaluation procedures.
- `Interpreters/`: IndiGolog interpreter, including transition systems available.
- `lib/`: Compatibility and tool libraries, global definitions.
- `Examples/`: Domain applications (e.g., elevator controller, Wumpus World)
  - Each example application has its own sub-directory. By convention, an application is loaded by  consulting a file with a name of the form `main_xxx.pl` where `xxx` denotes the specific Prolog platform (e.g., `main_swi.pl` for SWI-Prolog and `main_ecl.pl` for ECLIPSe).

## Examples

There are three example applications that work as simulations in console. You should try to run these before trying IndiGolog on a "real" external platform:

1. `Examples/Elevator-Vanilla`: This is the simplest application and it uses the vanilla IndiGolog interpreter (`indigolog-vanilla.pl`). It does not require any advanced features (like TCP/IP, Tcl/Tk, Java, or threads).
2. `Examples/ElevatorSim-BAT`: This is the same example as the vanilla one, but with a more sophisticated implementation.  There is now a special simulation environment that opens a new terminal window and a Tcl/Tk
interface where the user can enter exogenous actions asynchronously.
3. `Examples/ElevatorWumpus-KBAT`: This is the Wumpus World implementation. It uses a simulated world and results are displayed in a Java window.

## Contributors

IndiGolog is one of the many high-level agent-oriented programming languages developed by the Cognitive Robotics Group @ UofT under the direction of Hector Levesque and Ray Reiter.

* Sebastian Sardina (ssardina@cs.toronto.edu or ssardina@gmail.com)
* Hector Levesque
* Yves Lesperance
* Giuseppe De Giacomo
* Maurice Pagnucco
* Stavros Vassos
