This is the overview README file! Specific ones are also present in each folder.

The codebase is designed so that you can run it with minimal modifications. Once the working directory is set correctly, it should just work. The output from the simulations is expected to be in the same folder as the R scripts.

RAnalysisScripts contains, as the name implies, all my 'analysis' stuff - mostly this is in fact the figure plotting since we're working with simulation output not experimental data.
MATLABSimulations contains everything required to generate my 'data'. Slightly more tweaking is needed here - you may need to change the parameters to replicate each simulation. See the relevant readme in that folder for details.
AdditionalSimulations contains a range of extra setups, incorporating contexts such as post-infection mortality, an additional population sink, and a no-carry-capacity version. Not essential for replicating the work, but could
be of interest.

I've also commented as much as I can - at least where relevant to use.
Please note that some code will require a long time to fully execute. Heatmaps take a while to plot with my standard filesize, and the simulations can take hours if sufficiently large. I've tried to add progress indicators where
possible. The sample 'dataset' provided is intended as a timesaver - you may find it more informative (for the purposes of replication) to generate your own set via my simulations.