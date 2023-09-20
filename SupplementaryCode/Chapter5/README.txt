Some of these files are support files, and will have been present in the Chapter 4 too.
As an end user, the main file of interest is structsimreplicator.m, which produces my data. structsimsampler.m is also useful for producing the example networks I include in my thesis.
ode45eescript.m is used to get an 'exact' estimate of the endemic equilibrium. It is in fact an approximation, but a very good one. It is also fast, especially compared to the sim output.
A word of caution: the main replicator once again takes a long time to run, and thus should be handled carefully.