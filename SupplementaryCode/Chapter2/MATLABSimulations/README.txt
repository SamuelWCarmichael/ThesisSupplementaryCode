This is the MATLAB specific readme. For the overview readme, see the main folder.

This folder contains all required MATLAB scripts for generating my simulation output. Not all files need to be run, and some files require some tweaks depending on your chosen simulations.

No tweaks are required to:
- dediffModel.m
- retroModel.m
- noRetroModel.m
- Any of the 'simulateSandfly' set of files.
These should all operate normally from the start.

The others may require some minor tweaks.
- modelparams.m contains all the model parameters as the name suggests. They are commented, and should be configured to match whichever setup you wish.
- The 'collector' or 'replicator' files generate our simulation output. They should be mostly ready to run. You may wish to tweak the output filename, or simply do so after it is produced.

These simulation output files are stored initially in the same folder as the MATLAB scripts. Either move these to your chosen working directory for R or set it up accordingly.
NOTE: The output files can be large (>200MB). RStudio has no trouble loading them, though it can take a little time.
      Other software may struggle to load the entire file at once (tested EXCEL, which failed, and Notepad++, which struggled).