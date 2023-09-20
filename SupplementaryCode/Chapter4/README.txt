This holda all the simulations needed for Chapter 4. Most of either support functions (such as AinB etc.) or ones which I call in the main function.
As an end user, one can probably just run mainch3sim.m (the name is a legacy of the old naming scheme). It calls any functions it needs, and the settings are handled primarily in there.
RScripts once again includes everything needed to replicate my plots etc. It doesn't contain any data, however - you will need to generate your own.
A word of caution: the data generation, if run all at once, takes a LONG time (read: hours).