# Examples

There are three example applications that work as simulations in console. You should try to run these before trying IndiGolog on a "real" external platform:

1. `Examples/Elevator-Vanilla`: This is the simplest application and it uses the vanilla IndiGolog interpreter (`indigolog-vanilla.pl`). It does not require any advanced features (like TCP/IP, Tcl/Tk, Java, or threads).
2. `Examples/ElevatorSim-BAT`: This is the same example as the vanilla one, but with a more sophisticated implementation.  There is now a special simulation environment that opens a new terminal window and a Tcl/Tk
interface where the user can enter exogenous actions asynchronously.
3. `Examples/ElevatorWumpus-KBAT`: This is the Wumpus World implementation. It uses a simulated world and results are displayed in a Java window.
