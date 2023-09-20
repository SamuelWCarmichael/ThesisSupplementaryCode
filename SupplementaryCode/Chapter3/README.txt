This folder contains all the R scripts used for the survival analysis work. It doesn't include the data, which is publically available at the various sources specified in the thesis.

- BernCoxPHPlottingScript.R handles all the analysis of the Bern et al. dataset. It also plots the graphs.
- BrazilcombiPlotter.R handles all the Ribeiro et al. analysis, as well as the plotting of my multi-panel figures.
- PigottSAScript.R handles analysis of the Pigott et al. dataset. It also includes an alternative set of plots for the x-coordinate, even though we do not make use it (as it corresponds to longitude).

All scripts should work assuming you set the working directory properly, and place the data in the same folder as the script.