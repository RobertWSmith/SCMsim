SCMsim ReadMe
========================================================

This project is intended to run a supply chain simulation for raw materials. The end user will only be required to run the 'script.R' file within the /R directory. 

Objects
------------------------------

The main objects within the simulation are Raference Objects, and are in seperate files within the /R directory.
* Inventory Object, invRef.R
* Transit Object, transRef.R
* Hub Object, hubRef.R

Functions
-----------------------------

The main funcitons that users will want to call are within the simulation.R file
* simulate takes a inventory class and transit class as arguments and runs the basic simulation functions over one day
* simulate.hub take a hub class as its argument and simulates one day for the hub and customer factories
* simulation takes the predefined input, breaks it apart as appropriate, initializes and runs the simulation

In general, this has been written to run on any machine that has R installed, but may encounter errors. If you encounter an error, please email <rob_smith@goodyear.com> and include the call to traceback() to aide in fixing the issue.

This simulation is currently in development, changes may be occur at any time. Please check back frequently for the most up to date code.
