This directory contains the vanilla elevator examples for IndiGolog
that appeared in Reiter 2001 Knowledge in Action book, the ConGolog
paper in IJCAI-97, and other examples designed for IndiGolog by Hector
Levesque. To run it, load one of "main_swi" or "main_ecl" depending on 
which Prolog you have, call main/0, and you should see the output.  
This should work with SWI or ECL, not need TCP, Java or Wish!


Mini getting-started guide to get the Elevator vanilla examples:

1. Run your Prolog interpreter. For example "pl" for SWI Prolog.
   (Although the code was only tested in SWI Prolog, it should work with any
	reasonable implementation of Prolog)

2. Consult mainN_swi.pl, where N can be 1,2,3,4 or 5.

3. Post the goal "main".

4. The result will be displayed in the terminal. Exogenous actions or
	sensing results will also be queried in the terminal. 



For example, the following is a trace of a run of the smart controller
in the second example (running on SWI):

[ssardina@tea Elevator-Vanilla]$ pl
Welcome to SWI-Prolog (Multi-threaded, Version 5.6.24)
Copyright (c) 1990-2006 University of Amsterdam.
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [main2_swi].
%  ../../Interpreters/indigolog-vanilla compiled 0.01 sec, 14,900 bytes
%  example2 compiled 0.00 sec, 4,772 bytes
% main2_swi compiled 0.01 sec, 21,368 bytes

Yes
?- main.
Which controller do you want to run: dumb or smart? smart.

Executing controller: *smart*
up
up
open
close
off(5)
down
down
down
open
close
off(2)
down
open

13 actions.

Yes
?-





Another example using ECLIPSE Prolog now:

[ssardina@tea Elevator-Vanilla]$ eclipse
ECLiPSe Constraint Logic Programming System [kernel]
Kernel and basic libraries copyright Cisco Systems, Inc.
and subject to the Cisco-style Mozilla Public Licence 1.1
(see legal/cmpl.txt or www.eclipse-clp.org/licence)
Source available at www.sourceforge.org/projects/eclipse-clp
GMP library copyright Free Software Foundation, see legal/lgpl.txt
For other libraries see their individual copyright notices
Version 5.10 #44, Sun Jan 14 02:06 2007
[eclipse 1]: [main1_ecl].
indigolog-vanilla.pl compiled optimized 5344 bytes in 0.01 seconds
indigolog-vanilla_ecl.pl compiled optimized 0 bytes in 0.01 seconds
example1.pl compiled optimized 5724 bytes in 0.00 seconds
main1_ecl.pl compiled optimized 32 bytes in 0.01 seconds

Yes (0.01s cpu)
[eclipse 2]: main.
down
open
close
off(2)
up
up
up
open
close
off(5)
down
down
down
down
open

15 actions.

Yes (0.00s cpu, solution 1, maybe more) ?
[eclipse 3]:                                                         


