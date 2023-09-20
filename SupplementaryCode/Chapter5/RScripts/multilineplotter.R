# Graph plotting for the structured simulations. This one uses the new data, and also plots a log scale version.

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
library(viridis)
library(igraph)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/NetworkModelSection/ConnectednessR0EstimateSims/AnPredLimits/RScripts')

# This will probably boil down to a single graph, but it will depend on how the plotting goes.
# Lots of data to load this time around.
strucTSDat0 = read.csv('StructProp0TSOutput.csv',header=TRUE)
strucDDat0 = read.csv('StructProp0Diagnostics.csv',header=TRUE)
strucTSDat01 = read.csv('StructProp01TSOutput.csv',header=TRUE)
strucDDat01 = read.csv('StructProp01Diagnostics.csv',header=TRUE)
strucTSDat02 = read.csv('StructProp02TSOutput.csv',header=TRUE)
strucDDat02 = read.csv('StructProp02Diagnostics.csv',header=TRUE)
strucTSDat03 = read.csv('StructProp03TSOutput.csv',header=TRUE)
strucDDat03 = read.csv('StructProp03Diagnostics.csv',header=TRUE)
strucTSDat04 = read.csv('StructProp04TSOutput.csv',header=TRUE)
strucDDat04 = read.csv('StructProp04Diagnostics.csv',header=TRUE)
strucTSDat05 = read.csv('StructProp05TSOutput.csv',header=TRUE)
strucDDat05 = read.csv('StructProp05Diagnostics.csv',header=TRUE)
strucTSDat06 = read.csv('StructProp06TSOutput.csv',header=TRUE)
strucDDat06 = read.csv('StructProp06Diagnostics.csv',header=TRUE)
strucTSDat07 = read.csv('StructProp07TSOutput.csv',header=TRUE)
strucDDat07 = read.csv('StructProp07Diagnostics.csv',header=TRUE)
strucTSDat08 = read.csv('StructProp08TSOutput.csv',header=TRUE)
strucDDat08 = read.csv('StructProp08Diagnostics.csv',header=TRUE)
strucTSDat09 = read.csv('StructProp09TSOutput.csv',header=TRUE)
strucDDat09 = read.csv('StructProp09Diagnostics.csv',header=TRUE)
strucTSDat1 = read.csv('StructProp1TSOutput.csv',header=TRUE)
strucDDat1 = read.csv('StructProp1Diagnostics.csv',header=TRUE)

sp0tsdat = as.matrix(strucTSDat0)
sp01tsdat = as.matrix(strucTSDat01)
sp02tsdat = as.matrix(strucTSDat02)
sp03tsdat = as.matrix(strucTSDat03)
sp04tsdat = as.matrix(strucTSDat04)
sp05tsdat = as.matrix(strucTSDat05)
sp06tsdat = as.matrix(strucTSDat06)
sp07tsdat = as.matrix(strucTSDat07)
sp08tsdat = as.matrix(strucTSDat08)
sp09tsdat = as.matrix(strucTSDat09)
sp1tsdat = as.matrix(strucTSDat1)

# Each of these spots matrices is actually a different line. I want to create a lovely multi-line plot
# with a legend and things, but this needs careful handling.

mlparray = c(0,0,0) # A dummy row of my data array, it will hold all the data we process (and we'll cleave this row).
rVec = c(0.008,0.0087,0.0094,0.0101,0.0108,0.0115,0.0122,0.0129,0.0136,0.0143,0.015) # Diagnostic back-up.
gam = 0.025 # More stuff from diagnostics.

## We do this one at a time, conglomerate everything where possible and at the end if not. Then we plot it
## all at once, with maybe some smaller plots if of interest.

# sp0tsdat first, the fully structured sim. This one will have complications, the endemic equilibrium is really close
# to 0, but is analytically stable... just tends to go extinct in my sims.

sp0tsr1 = sp0tsdat[1:50,]
sp0tsr2 = sp0tsdat[51:100,]
sp0tsr3 = sp0tsdat[101:150,]
sp0tsr4 = sp0tsdat[151:200,]
sp0tsr5 = sp0tsdat[201:250,]
sp0tsr6 = sp0tsdat[251:300,]
sp0tsr7 = sp0tsdat[301:350,]
sp0tsr8 = sp0tsdat[351:400,]
sp0tsr9 = sp0tsdat[401:450,]
sp0tsr10 = sp0tsdat[451:500,]
sp0tsr11 = sp0tsdat[501:550,]

sp0ddr1 = strucDDat0[1:50,]
sp0ddr2 = strucDDat0[51:100,]
sp0ddr3 = strucDDat0[101:150,]
sp0ddr4 = strucDDat0[151:200,]
sp0ddr5 = strucDDat0[201:250,]
sp0ddr6 = strucDDat0[251:300,]
sp0ddr7 = strucDDat0[301:350,]
sp0ddr8 = strucDDat0[351:400,]
sp0ddr9 = strucDDat0[401:450,]
sp0ddr10 = strucDDat0[451:500,]
sp0ddr11 = strucDDat0[501:550,]

svec = apply(sp0tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr1 = sp0tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr1 = sp0ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr2 = sp0tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr2 = sp0ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr3 = sp0tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr3 = sp0ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr4 = sp0tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr4 = sp0ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr5 = sp0tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr5 = sp0ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr6 = sp0tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr6 = sp0ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr7 = sp0tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr7 = sp0ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr8 = sp0tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr8 = sp0ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr9 = sp0tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr9 = sp0ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr10 = sp0tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr10 = sp0ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp0tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp0tsr11 = sp0tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp0ddr11 = sp0ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp0tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp0tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp0tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp0tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp0tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp0tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp0tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp0tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp0tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp0tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp0tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp0tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp0tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp0tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp0tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp0tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp0tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp0tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp0tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp0tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp0tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp0tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp0bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp0bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp0bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp0bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp0bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp0bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp0bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp0bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp0bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp0bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp0bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,1))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,1))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,1))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,1))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,1))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,1))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,1))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,1))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,1))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,1))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,1))

# Now to repeat for all the other prop runs. One at a time, until I have a full set.

## sp01 next, 10% randomised.

sp01tsr1 = sp01tsdat[1:50,]
sp01tsr2 = sp01tsdat[51:100,]
sp01tsr3 = sp01tsdat[101:150,]
sp01tsr4 = sp01tsdat[151:200,]
sp01tsr5 = sp01tsdat[201:250,]
sp01tsr6 = sp01tsdat[251:300,]
sp01tsr7 = sp01tsdat[301:350,]
sp01tsr8 = sp01tsdat[351:400,]
sp01tsr9 = sp01tsdat[401:450,]
sp01tsr10 = sp01tsdat[451:500,]
sp01tsr11 = sp01tsdat[501:550,]

sp01ddr1 = strucDDat01[1:50,]
sp01ddr2 = strucDDat01[51:100,]
sp01ddr3 = strucDDat01[101:150,]
sp01ddr4 = strucDDat01[151:200,]
sp01ddr5 = strucDDat01[201:250,]
sp01ddr6 = strucDDat01[251:300,]
sp01ddr7 = strucDDat01[301:350,]
sp01ddr8 = strucDDat01[351:400,]
sp01ddr9 = strucDDat01[401:450,]
sp01ddr10 = strucDDat01[451:500,]
sp01ddr11 = strucDDat01[501:550,]

svec = apply(sp01tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr1 = sp01tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr1 = sp01ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr2 = sp01tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr2 = sp01ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr3 = sp01tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr3 = sp01ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr4 = sp01tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr4 = sp01ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr5 = sp01tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr5 = sp01ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr6 = sp01tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr6 = sp01ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr7 = sp01tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr7 = sp01ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr8 = sp01tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr8 = sp01ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr9 = sp01tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr9 = sp01ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr10 = sp01tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr10 = sp01ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp01tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp01tsr11 = sp01tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp01ddr11 = sp01ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp01tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp01tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp01tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp01tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp01tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp01tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp01tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp01tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp01tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp01tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp01tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp01tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp01tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp01tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp01tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp01tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp01tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp01tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp01tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp01tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp01tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp01tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp01bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp01bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp01bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp01bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp01bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp01bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp01bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp01bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp01bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp01bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp01bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,2))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,2))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,2))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,2))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,2))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,2))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,2))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,2))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,2))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,2))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,2))

# sp02, 20% randomised.

sp02tsr1 = sp02tsdat[1:50,]
sp02tsr2 = sp02tsdat[51:100,]
sp02tsr3 = sp02tsdat[101:150,]
sp02tsr4 = sp02tsdat[151:200,]
sp02tsr5 = sp02tsdat[201:250,]
sp02tsr6 = sp02tsdat[251:300,]
sp02tsr7 = sp02tsdat[301:350,]
sp02tsr8 = sp02tsdat[351:400,]
sp02tsr9 = sp02tsdat[401:450,]
sp02tsr10 = sp02tsdat[451:500,]
sp02tsr11 = sp02tsdat[501:550,]

sp02ddr1 = strucDDat02[1:50,]
sp02ddr2 = strucDDat02[51:100,]
sp02ddr3 = strucDDat02[101:150,]
sp02ddr4 = strucDDat02[151:200,]
sp02ddr5 = strucDDat02[201:250,]
sp02ddr6 = strucDDat02[251:300,]
sp02ddr7 = strucDDat02[301:350,]
sp02ddr8 = strucDDat02[351:400,]
sp02ddr9 = strucDDat02[401:450,]
sp02ddr10 = strucDDat02[451:500,]
sp02ddr11 = strucDDat02[501:550,]

svec = apply(sp02tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr1 = sp02tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr1 = sp02ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr2 = sp02tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr2 = sp02ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr3 = sp02tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr3 = sp02ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr4 = sp02tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr4 = sp02ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr5 = sp02tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr5 = sp02ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr6 = sp02tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr6 = sp02ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr7 = sp02tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr7 = sp02ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr8 = sp02tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr8 = sp02ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr9 = sp02tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr9 = sp02ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr10 = sp02tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr10 = sp02ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp02tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp02tsr11 = sp02tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp02ddr11 = sp02ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp02tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp02tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp02tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp02tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp02tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp02tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp02tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp02tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp02tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp02tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp02tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp02tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp02tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp02tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp02tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp02tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp02tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp02tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp02tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp02tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp02tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp02tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp02bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp02bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp02bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp02bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp02bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp02bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp02bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp02bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp02bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp02bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp02bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,3))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,3))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,3))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,3))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,3))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,3))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,3))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,3))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,3))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,3))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,3))

# sp03, 30% Randomised

sp03tsr1 = sp03tsdat[1:50,]
sp03tsr2 = sp03tsdat[51:100,]
sp03tsr3 = sp03tsdat[101:150,]
sp03tsr4 = sp03tsdat[151:200,]
sp03tsr5 = sp03tsdat[201:250,]
sp03tsr6 = sp03tsdat[251:300,]
sp03tsr7 = sp03tsdat[301:350,]
sp03tsr8 = sp03tsdat[351:400,]
sp03tsr9 = sp03tsdat[401:450,]
sp03tsr10 = sp03tsdat[451:500,]
sp03tsr11 = sp03tsdat[501:550,]

sp03ddr1 = strucDDat03[1:50,]
sp03ddr2 = strucDDat03[51:100,]
sp03ddr3 = strucDDat03[101:150,]
sp03ddr4 = strucDDat03[151:200,]
sp03ddr5 = strucDDat03[201:250,]
sp03ddr6 = strucDDat03[251:300,]
sp03ddr7 = strucDDat03[301:350,]
sp03ddr8 = strucDDat03[351:400,]
sp03ddr9 = strucDDat03[401:450,]
sp03ddr10 = strucDDat03[451:500,]
sp03ddr11 = strucDDat03[501:550,]

svec = apply(sp03tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr1 = sp03tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr1 = sp03ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr2 = sp03tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr2 = sp03ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr3 = sp03tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr3 = sp03ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr4 = sp03tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr4 = sp03ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr5 = sp03tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr5 = sp03ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr6 = sp03tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr6 = sp03ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr7 = sp03tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr7 = sp03ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr8 = sp03tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr8 = sp03ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr9 = sp03tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr9 = sp03ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr10 = sp03tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr10 = sp03ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp03tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp03tsr11 = sp03tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp03ddr11 = sp03ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp03tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp03tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp03tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp03tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp03tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp03tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp03tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp03tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp03tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp03tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp03tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp03tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp03tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp03tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp03tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp03tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp03tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp03tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp03tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp03tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp03tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp03tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp03bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp03bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp03bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp03bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp03bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp03bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp03bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp03bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp03bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp03bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp03bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,4))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,4))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,4))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,4))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,4))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,4))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,4))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,4))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,4))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,4))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,4))

# sp04, 40% Randomised

sp04tsr1 = sp04tsdat[1:50,]
sp04tsr2 = sp04tsdat[51:100,]
sp04tsr3 = sp04tsdat[101:150,]
sp04tsr4 = sp04tsdat[151:200,]
sp04tsr5 = sp04tsdat[201:250,]
sp04tsr6 = sp04tsdat[251:300,]
sp04tsr7 = sp04tsdat[301:350,]
sp04tsr8 = sp04tsdat[351:400,]
sp04tsr9 = sp04tsdat[401:450,]
sp04tsr10 = sp04tsdat[451:500,]
sp04tsr11 = sp04tsdat[501:550,]

sp04ddr1 = strucDDat04[1:50,]
sp04ddr2 = strucDDat04[51:100,]
sp04ddr3 = strucDDat04[101:150,]
sp04ddr4 = strucDDat04[151:200,]
sp04ddr5 = strucDDat04[201:250,]
sp04ddr6 = strucDDat04[251:300,]
sp04ddr7 = strucDDat04[301:350,]
sp04ddr8 = strucDDat04[351:400,]
sp04ddr9 = strucDDat04[401:450,]
sp04ddr10 = strucDDat04[451:500,]
sp04ddr11 = strucDDat04[501:550,]

svec = apply(sp04tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr1 = sp04tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr1 = sp04ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr2 = sp04tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr2 = sp04ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr3 = sp04tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr3 = sp04ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr4 = sp04tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr4 = sp04ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr5 = sp04tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr5 = sp04ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr6 = sp04tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr6 = sp04ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr7 = sp04tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr7 = sp04ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr8 = sp04tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr8 = sp04ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr9 = sp04tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr9 = sp04ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr10 = sp04tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr10 = sp04ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp04tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp04tsr11 = sp04tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp04ddr11 = sp04ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp04tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp04tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp04tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp04tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp04tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp04tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp04tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp04tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp04tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp04tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp04tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp04tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp04tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp04tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp04tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp04tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp04tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp04tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp04tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp04tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp04tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp04tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp04bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp04bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp04bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp04bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp04bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp04bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp04bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp04bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp04bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp04bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp04bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,5))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,5))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,5))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,5))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,5))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,5))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,5))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,5))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,5))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,5))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,5))

# sp05, 50% Randomised

sp05tsr1 = sp05tsdat[1:50,]
sp05tsr2 = sp05tsdat[51:100,]
sp05tsr3 = sp05tsdat[101:150,]
sp05tsr4 = sp05tsdat[151:200,]
sp05tsr5 = sp05tsdat[201:250,]
sp05tsr6 = sp05tsdat[251:300,]
sp05tsr7 = sp05tsdat[301:350,]
sp05tsr8 = sp05tsdat[351:400,]
sp05tsr9 = sp05tsdat[401:450,]
sp05tsr10 = sp05tsdat[451:500,]
sp05tsr11 = sp05tsdat[501:550,]

sp05ddr1 = strucDDat05[1:50,]
sp05ddr2 = strucDDat05[51:100,]
sp05ddr3 = strucDDat05[101:150,]
sp05ddr4 = strucDDat05[151:200,]
sp05ddr5 = strucDDat05[201:250,]
sp05ddr6 = strucDDat05[251:300,]
sp05ddr7 = strucDDat05[301:350,]
sp05ddr8 = strucDDat05[351:400,]
sp05ddr9 = strucDDat05[401:450,]
sp05ddr10 = strucDDat05[451:500,]
sp05ddr11 = strucDDat05[501:550,]

svec = apply(sp05tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr1 = sp05tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr1 = sp05ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr2 = sp05tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr2 = sp05ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr3 = sp05tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr3 = sp05ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr4 = sp05tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr4 = sp05ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr5 = sp05tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr5 = sp05ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr6 = sp05tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr6 = sp05ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr7 = sp05tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr7 = sp05ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr8 = sp05tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr8 = sp05ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr9 = sp05tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr9 = sp05ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr10 = sp05tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr10 = sp05ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp05tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp05tsr11 = sp05tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp05ddr11 = sp05ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp05tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp05tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp05tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp05tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp05tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp05tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp05tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp05tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp05tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp05tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp05tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp05tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp05tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp05tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp05tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp05tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp05tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp05tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp05tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp05tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp05tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp05tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp05bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp05bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp05bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp05bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp05bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp05bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp05bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp05bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp05bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp05bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp05bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,6))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,6))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,6))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,6))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,6))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,6))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,6))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,6))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,6))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,6))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,6))

# sp06, 60% Randomised

sp06tsr1 = sp06tsdat[1:50,]
sp06tsr2 = sp06tsdat[51:100,]
sp06tsr3 = sp06tsdat[101:150,]
sp06tsr4 = sp06tsdat[151:200,]
sp06tsr5 = sp06tsdat[201:250,]
sp06tsr6 = sp06tsdat[251:300,]
sp06tsr7 = sp06tsdat[301:350,]
sp06tsr8 = sp06tsdat[351:400,]
sp06tsr9 = sp06tsdat[401:450,]
sp06tsr10 = sp06tsdat[451:500,]
sp06tsr11 = sp06tsdat[501:550,]

sp06ddr1 = strucDDat06[1:50,]
sp06ddr2 = strucDDat06[51:100,]
sp06ddr3 = strucDDat06[101:150,]
sp06ddr4 = strucDDat06[151:200,]
sp06ddr5 = strucDDat06[201:250,]
sp06ddr6 = strucDDat06[251:300,]
sp06ddr7 = strucDDat06[301:350,]
sp06ddr8 = strucDDat06[351:400,]
sp06ddr9 = strucDDat06[401:450,]
sp06ddr10 = strucDDat06[451:500,]
sp06ddr11 = strucDDat06[501:550,]

svec = apply(sp06tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr1 = sp06tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr1 = sp06ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr2 = sp06tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr2 = sp06ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr3 = sp06tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr3 = sp06ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr4 = sp06tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr4 = sp06ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr5 = sp06tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr5 = sp06ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr6 = sp06tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr6 = sp06ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr7 = sp06tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr7 = sp06ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr8 = sp06tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr8 = sp06ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr9 = sp06tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr9 = sp06ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr10 = sp06tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr10 = sp06ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp06tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp06tsr11 = sp06tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp06ddr11 = sp06ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp06tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp06tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp06tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp06tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp06tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp06tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp06tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp06tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp06tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp06tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp06tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp06tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp06tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp06tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp06tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp06tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp06tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp06tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp06tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp06tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp06tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp06tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp06bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp06bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp06bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp06bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp06bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp06bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp06bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp06bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp06bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp06bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp06bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,7))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,7))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,7))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,7))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,7))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,7))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,7))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,7))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,7))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,7))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,7))

# sp08, 80% Randomised

sp08tsr1 = sp08tsdat[1:50,]
sp08tsr2 = sp08tsdat[51:100,]
sp08tsr3 = sp08tsdat[101:150,]
sp08tsr4 = sp08tsdat[151:200,]
sp08tsr5 = sp08tsdat[201:250,]
sp08tsr6 = sp08tsdat[251:300,]
sp08tsr7 = sp08tsdat[301:350,]
sp08tsr8 = sp08tsdat[351:400,]
sp08tsr9 = sp08tsdat[401:450,]
sp08tsr10 = sp08tsdat[451:500,]
sp08tsr11 = sp08tsdat[501:550,]

sp08ddr1 = strucDDat08[1:50,]
sp08ddr2 = strucDDat08[51:100,]
sp08ddr3 = strucDDat08[101:150,]
sp08ddr4 = strucDDat08[151:200,]
sp08ddr5 = strucDDat08[201:250,]
sp08ddr6 = strucDDat08[251:300,]
sp08ddr7 = strucDDat08[301:350,]
sp08ddr8 = strucDDat08[351:400,]
sp08ddr9 = strucDDat08[401:450,]
sp08ddr10 = strucDDat08[451:500,]
sp08ddr11 = strucDDat08[501:550,]

svec = apply(sp08tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr1 = sp08tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr1 = sp08ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr2 = sp08tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr2 = sp08ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr3 = sp08tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr3 = sp08ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr4 = sp08tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr4 = sp08ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr5 = sp08tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr5 = sp08ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr6 = sp08tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr6 = sp08ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr7 = sp08tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr7 = sp08ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr8 = sp08tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr8 = sp08ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr9 = sp08tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr9 = sp08ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr10 = sp08tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr10 = sp08ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp08tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp08tsr11 = sp08tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp08ddr11 = sp08ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp08tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp08tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp08tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp08tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp08tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp08tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp08tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp08tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp08tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp08tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp08tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp08tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp08tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp08tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp08tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp08tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp08tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp08tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp08tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp08tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp08tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp08tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp08bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp08bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp08bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp08bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp08bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp08bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp08bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp08bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp08bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp08bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp08bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,9))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,9))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,9))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,9))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,9))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,9))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,9))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,9))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,9))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,9))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,9))

# sp09, 90% Randomised

sp09tsr1 = sp09tsdat[1:50,]
sp09tsr2 = sp09tsdat[51:100,]
sp09tsr3 = sp09tsdat[101:150,]
sp09tsr4 = sp09tsdat[151:200,]
sp09tsr5 = sp09tsdat[201:250,]
sp09tsr6 = sp09tsdat[251:300,]
sp09tsr7 = sp09tsdat[301:350,]
sp09tsr8 = sp09tsdat[351:400,]
sp09tsr9 = sp09tsdat[401:450,]
sp09tsr10 = sp09tsdat[451:500,]
sp09tsr11 = sp09tsdat[501:550,]

sp09ddr1 = strucDDat09[1:50,]
sp09ddr2 = strucDDat09[51:100,]
sp09ddr3 = strucDDat09[101:150,]
sp09ddr4 = strucDDat09[151:200,]
sp09ddr5 = strucDDat09[201:250,]
sp09ddr6 = strucDDat09[251:300,]
sp09ddr7 = strucDDat09[301:350,]
sp09ddr8 = strucDDat09[351:400,]
sp09ddr9 = strucDDat09[401:450,]
sp09ddr10 = strucDDat09[451:500,]
sp09ddr11 = strucDDat09[501:550,]

svec = apply(sp09tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr1 = sp09tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr1 = sp09ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr2 = sp09tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr2 = sp09ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr3 = sp09tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr3 = sp09ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr4 = sp09tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr4 = sp09ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr5 = sp09tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr5 = sp09ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr6 = sp09tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr6 = sp09ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr7 = sp09tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr7 = sp09ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr8 = sp09tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr8 = sp09ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr9 = sp09tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr9 = sp09ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr10 = sp09tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr10 = sp09ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp09tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp09tsr11 = sp09tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp09ddr11 = sp09ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp09tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp09tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp09tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp09tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp09tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp09tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp09tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp09tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp09tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp09tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp09tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp09tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp09tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp09tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp09tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp09tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp09tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp09tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp09tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp09tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp09tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp09tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp09bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp09bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp09bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp09bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp09bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp09bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp09bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp09bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp09bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp09bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp09bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,10))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,10))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,10))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,10))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,10))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,10))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,10))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,10))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,10))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,10))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,10))

# Finally, sp1, 100% randomised.

sp1tsr1 = sp1tsdat[1:50,]
sp1tsr2 = sp1tsdat[51:100,]
sp1tsr3 = sp1tsdat[101:150,]
sp1tsr4 = sp1tsdat[151:200,]
sp1tsr5 = sp1tsdat[201:250,]
sp1tsr6 = sp1tsdat[251:300,]
sp1tsr7 = sp1tsdat[301:350,]
sp1tsr8 = sp1tsdat[351:400,]
sp1tsr9 = sp1tsdat[401:450,]
sp1tsr10 = sp1tsdat[451:500,]
sp1tsr11 = sp1tsdat[501:550,]

sp1ddr1 = strucDDat1[1:50,]
sp1ddr2 = strucDDat1[51:100,]
sp1ddr3 = strucDDat1[101:150,]
sp1ddr4 = strucDDat1[151:200,]
sp1ddr5 = strucDDat1[201:250,]
sp1ddr6 = strucDDat1[251:300,]
sp1ddr7 = strucDDat1[301:350,]
sp1ddr8 = strucDDat1[351:400,]
sp1ddr9 = strucDDat1[401:450,]
sp1ddr10 = strucDDat1[451:500,]
sp1ddr11 = strucDDat1[501:550,]

svec = apply(sp1tsr1,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr1 = sp1tsr1[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr1 = sp1ddr1[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr2,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr2 = sp1tsr2[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr2 = sp1ddr2[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr3,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr3 = sp1tsr3[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr3 = sp1ddr3[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr4,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr4 = sp1tsr4[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr4 = sp1ddr4[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr5,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr5 = sp1tsr5[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr5 = sp1ddr5[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr6,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr6 = sp1tsr6[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr6 = sp1ddr6[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr7,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr7 = sp1tsr7[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr7 = sp1ddr7[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr8,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr8 = sp1tsr8[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr8 = sp1ddr8[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr9,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr9 = sp1tsr9[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr9 = sp1ddr9[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr10,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr10 = sp1tsr10[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr10 = sp1ddr10[svec > 0,] # We also purge the extinct sim diagnostic data.

svec = apply(sp1tsr11,1,min) # This identifies the lowest value, which will be 0 for sims that went extinct.
sp1tsr11 = sp1tsr11[svec > 0,1501:2000] # In preparation for curation, we remove the extinct sims and extract the last half of the timeseries.
sp1ddr11 = sp1ddr11[svec > 0,] # We also purge the extinct sim diagnostic data.

# Given the propensity for all reps in a given runset to end up extinct, we need a checker in place.
# We also no longer need the analytic estimate calculator here, because we always use the same type of network
# and thus calculate it at the end instead.

if(length(sp1tsr1[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep1mee = 0
  rep1se = 0
}else{
  eevecr1 = apply(sp1tsr1,1,mean)
  eevecr1 = eevecr1/1200
  rep1mee = mean(eevecr1)
  rep1se = sqrt(var(eevecr1))
}

if(length(sp1tsr2[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep2mee = 0
  rep2se = 0
}else{
  eevecr2 = apply(sp1tsr2,1,mean)
  eevecr2 = eevecr2/1200
  rep2mee = mean(eevecr2)
  rep2se = sqrt(var(eevecr2))
}

if(length(sp1tsr3[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep3mee = 0
  rep3se = 0
}else{
  eevecr3 = apply(sp1tsr3,1,mean)
  eevecr3 = eevecr3/1200
  rep3mee = mean(eevecr3)
  rep3se = sqrt(var(eevecr3))
}

if(length(sp1tsr4[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep4mee = 0
  rep4se = 0
}else{
  eevecr4 = apply(sp1tsr4,1,mean)
  eevecr4 = eevecr4/1200
  rep4mee = mean(eevecr4)
  rep4se = sqrt(var(eevecr4))
}

if(length(sp1tsr5[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep5mee = 0
  rep5se = 0
}else{
  eevecr5 = apply(sp1tsr5,1,mean)
  eevecr5 = eevecr5/1200
  rep5mee = mean(eevecr5)
  rep5se = sqrt(var(eevecr5))
}

if(length(sp1tsr6[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep6mee = 0
  rep6se = 0
}else{
  eevecr6 = apply(sp1tsr6,1,mean)
  eevecr6 = eevecr6/1200
  rep6mee = mean(eevecr6)
  rep6se = sqrt(var(eevecr6))
}

if(length(sp1tsr7[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep7mee = 0
  rep7se = 0
}else{
  eevecr7 = apply(sp1tsr7,1,mean)
  eevecr7 = eevecr7/1200
  rep7mee = mean(eevecr7)
  rep7se = sqrt(var(eevecr7))
}

if(length(sp1tsr8[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep8mee = 0
  rep8se = 0
}else{
  eevecr8 = apply(sp1tsr8,1,mean)
  eevecr8 = eevecr8/1200
  rep8mee = mean(eevecr8)
  rep8se = sqrt(var(eevecr8))
}

if(length(sp1tsr9[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep9mee = 0
  rep9se = 0
}else{
  eevecr9 = apply(sp1tsr9,1,mean)
  eevecr9 = eevecr9/1200
  rep9mee = mean(eevecr9)
  rep9se = sqrt(var(eevecr9))
}

if(length(sp1tsr10[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep10mee = 0
  rep10se = 0
}else{
  eevecr10 = apply(sp1tsr10,1,mean)
  eevecr10 = eevecr10/1200
  rep10mee = mean(eevecr10)
  rep10se = sqrt(var(eevecr10))
}

if(length(sp1tsr11[,1])<2){
  # If this is true, we can't properly calculate the var etc. so we set the point to be at 0.
  rep11mee = 0
  rep11se = 0
}else{
  eevecr11 = apply(sp1tsr11,1,mean)
  eevecr11 = eevecr11/1200
  rep11mee = mean(eevecr11)
  rep11se = sqrt(var(eevecr11))
}

# This next section is prep for plotting error bars, but I don't currently use it.

sp1bp1 = data.frame(
  x = rVec[1]/gam,
  lw = rep1mee - rep1se,
  me = rep1mee,
  uw = rep1mee + rep1se
)

sp1bp2 = data.frame(
  x = rVec[2]/gam,
  lw = rep2mee - rep2se,
  me = rep2mee,
  uw = rep2mee + rep2se
)

sp1bp3 = data.frame(
  x = rVec[3]/gam,
  lw = rep3mee - rep3se,
  me = rep3mee,
  uw = rep3mee + rep3se
)

sp1bp4 = data.frame(
  x = rVec[4]/gam,
  lw = rep4mee - rep4se,
  me = rep4mee,
  uw = rep4mee + rep4se
)

sp1bp5 = data.frame(
  x = rVec[5]/gam,
  lw = rep5mee - rep5se,
  me = rep5mee,
  uw = rep5mee + rep5se
)

sp1bp6 = data.frame(
  x = rVec[6]/gam,
  lw = rep6mee - rep6se,
  me = rep6mee,
  uw = rep6mee + rep6se
)

sp1bp7 = data.frame(
  x = rVec[7]/gam,
  lw = rep7mee - rep7se,
  me = rep7mee,
  uw = rep7mee + rep7se
)

sp1bp8 = data.frame(
  x = rVec[8]/gam,
  lw = rep8mee - rep8se,
  me = rep8mee,
  uw = rep8mee + rep8se
)

sp1bp9 = data.frame(
  x = rVec[9]/gam,
  lw = rep9mee - rep9se,
  me = rep9mee,
  uw = rep9mee + rep9se
)

sp1bp10 = data.frame(
  x = rVec[10]/gam,
  lw = rep10mee - rep10se,
  me = rep10mee,
  uw = rep10mee + rep10se
)

sp1bp11 = data.frame(
  x = rVec[11]/gam,
  lw = rep11mee - rep11se,
  me = rep11mee,
  uw = rep11mee + rep11se
)

# We want to create a 3-column array, 1 for mean, 1 for the R/Gamma, 1 for the Group.

mlparray = rbind(mlparray, c(rep1mee,rVec[1]/gam,11))
mlparray = rbind(mlparray, c(rep2mee,rVec[2]/gam,11))
mlparray = rbind(mlparray, c(rep3mee,rVec[3]/gam,11))
mlparray = rbind(mlparray, c(rep4mee,rVec[4]/gam,11))
mlparray = rbind(mlparray, c(rep5mee,rVec[5]/gam,11))
mlparray = rbind(mlparray, c(rep6mee,rVec[6]/gam,11))
mlparray = rbind(mlparray, c(rep7mee,rVec[7]/gam,11))
mlparray = rbind(mlparray, c(rep8mee,rVec[8]/gam,11))
mlparray = rbind(mlparray, c(rep9mee,rVec[9]/gam,11))
mlparray = rbind(mlparray, c(rep10mee,rVec[10]/gam,11))
mlparray = rbind(mlparray, c(rep11mee,rVec[11]/gam,11))

# Now, we need a little more prep work.
mlparray = mlparray[-1,] # Remove the original dummy row from mlparray.
mlpframe = data.frame(mlparray)
names(mlpframe) = c('y','x','group')

# We also want to generate the analytic approximation for this.

# Regenerating the diagnostic info to restore trimmed entries.
sp0ddr1 = strucDDat0[1:50,]
sp0ddr2 = strucDDat0[51:100,]
sp0ddr3 = strucDDat0[101:150,]
sp0ddr4 = strucDDat0[151:200,]
sp0ddr5 = strucDDat0[201:250,]
sp0ddr6 = strucDDat0[251:300,]
sp0ddr7 = strucDDat0[301:350,]
sp0ddr8 = strucDDat0[351:400,]
sp0ddr9 = strucDDat0[401:450,]
sp0ddr10 = strucDDat0[451:500,]
sp0ddr11 = strucDDat0[501:550,]

# Attempt 1: We use just the diagnostic data from the sp0 set, since all sps are the same.

afp1 = mean(sp0ddr1$EEIProp)
afp2 = mean(sp0ddr2$EEIProp)
afp3 = mean(sp0ddr3$EEIProp)
afp4 = mean(sp0ddr4$EEIProp)
afp5 = mean(sp0ddr5$EEIProp)
afp6 = mean(sp0ddr6$EEIProp)
afp7 = mean(sp0ddr7$EEIProp)
afp8 = mean(sp0ddr8$EEIProp)
afp9 = mean(sp0ddr9$EEIProp)
afp10 = mean(sp0ddr10$EEIProp)
afp11 = mean(sp0ddr11$EEIProp)

anarray = c(afp1,rVec[1]/gam)
anarray = rbind(anarray,c(afp2,rVec[2]/gam))
anarray = rbind(anarray,c(afp3,rVec[3]/gam))
anarray = rbind(anarray,c(afp4,rVec[4]/gam))
anarray = rbind(anarray,c(afp5,rVec[5]/gam))
anarray = rbind(anarray,c(afp6,rVec[6]/gam))
anarray = rbind(anarray,c(afp7,rVec[7]/gam))
anarray = rbind(anarray,c(afp8,rVec[8]/gam))
anarray = rbind(anarray,c(afp9,rVec[9]/gam))
anarray = rbind(anarray,c(afp10,rVec[10]/gam))
anarray = rbind(anarray,c(afp11,rVec[11]/gam))

anframe = data.frame(anarray)
names(anframe) = c('y','x')

# Slightly experimental, but I'm going to try converting my group category into a factor column.
# Might just break things, but we'll see. Also going to tweak the values and names a little
# so that the legend works with minimal fuss... rather than the horror it is now.

mlpframe$group = (11-mlpframe$group)/10
mlpframe$group = as.factor(mlpframe$group)
names(mlpframe) = c('y','x','Struct')

# Finally, we can plot.

multilineplotv1 = ggplot()+
  geom_line(data = mlpframe, aes(x = x, y = y, group = Struct, colour = Struct),size=1.0)+
  geom_line(data = anframe, aes(x = x, y = y),colour = 'black',size=1.4)+
  scale_color_viridis(discrete = T, option = "C")+
  xlab('Infection/Recovery Rate')+
  ylab('Proportion Infected Individuals')+
  ylim(0.0,0.6)+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

mlplogframe = mlpframe
mlplogframe$y = log(mlplogframe$y)
anlogframe = anframe
anlogframe$y = log(anlogframe$y)

logmultilineplotv1 = ggplot()+
  geom_line(data = mlplogframe, aes(x = x, y = y, group = Struct, colour = Struct),size=1.0)+
  geom_line(data = anlogframe, aes(x = x, y = y),colour = 'red',size=1.4)+
  scale_color_viridis(discrete = T, option = "D")+
  xlab('Infection/Recovery Rate')+
  ylab('Log(Proportion Infected Individuals)')+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15))
