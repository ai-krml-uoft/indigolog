In order to get the Wumpus example running do the following:

1. Make sure the installation of IndiGolog is correct, including var PATH_INDIGOLOG
	(read install.txt at the root directory of IndiGolog)

2. Make sure you have xterm installed; it will be used by several device managers.

3. Install a Java Runtime Environment (JRE) to your system.
   Make sure that 'java' invokes the Java application launcher.
   Also, note that JRE1.5 from SUN is tested to work, while the GNU Java 
   Compiler (GJC) is reported to fail when executing the script at step 3.

4. Go to directory <mypath>/indigolog-linux-vvv/Examples/Wumpus-KBAT/WumpusApplet
   and execute script startWumpusGUI.

5. Go to directory <mypath>/indigolog/Examples/Wumpus-KBAT and start SWI Prolog with
	sufficient global & heap memory: pl -L128m -G128m -T64m

6. Consult main_swi.pl and execute goal "main". Several controllers will be offered.
	Enter controller number "4" as the agent controller.
   That is, in the Prolog prompt that appears do the following:
       ?- [main_swi].
       ?- main.
	Available Controllers: [3, 4, 5, 6, 1, 2]
	Which controller do you want to execute?
       4.		<<< TYPE "4." and ENTER

7. Watch the IndiGolog Agent act in the Wumpus World applet.

8. To run it once again, execute goal "main" again.
