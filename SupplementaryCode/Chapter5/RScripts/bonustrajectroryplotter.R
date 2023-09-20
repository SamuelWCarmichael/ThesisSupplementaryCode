# A few other plots that are largely to aid in interpretation, rather than presenting our analysis.

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
library(viridis)
library(igraph)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/NetworkModelSection/ConnectednessR0EstimateSims/AnPredLimits/RScripts')

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

# I want to plot some example trajectories. I was thinking maybe just a random high, medium and low transmission
# rate trajectory. Make them distinct and shown the full trajectory. Perhaps 3 figures for different structuredness
# values. I can put one in the text and maybe the others in a supplementary if needed.

# For now I'll start with the min structuredness sims.

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

ind = round(runif(1,min = 0.5,max=50.49))
lowRminS = sp1tsdat[ind+50,]

ind = round(runif(1,min = 0.5,max=50.49))
modRminS = sp1tsdat[ind+250,]

ind = round(runif(1,min = 0.5,max=50.49))
maxRminS = sp1tsdat[ind+500,]

t = 1:2000

minSplotdat1 = cbind(t,lowRminS,0.0087)
minSplotdat2 = cbind(t,modRminS,0.0115)
minSplotdat3 = cbind(t,maxRminS,0.015)
minSplotdat = rbind(minSplotdat1,minSplotdat2,minSplotdat3)

minSplotdat = data.frame(minSplotdat)
names(minSplotdat) = c("t","PropInf","TRate")
minSplotdat$TRate = as.factor(minSplotdat$TRate)

minStrajplot = ggplot()+
  geom_line(data = minSplotdat, aes(x = t, y = PropInf,group = TRate,colour = TRate))+
  ylab("Concurrent Infections")+
  xlab("Time")+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 15))

ind = round(runif(1,min = 0.5,max=50.49))
lowRmidS = sp05tsdat[ind+50,]

ind = round(runif(1,min = 0.5,max=50.49))
modRmidS = sp05tsdat[ind+250,]

ind = round(runif(1,min = 0.5,max=50.49))
maxRmidS = sp05tsdat[ind+500,]

t = 1:2000

midSplotdat1 = cbind(t,lowRmidS,0.0087)
midSplotdat2 = cbind(t,modRmidS,0.0115)
midSplotdat3 = cbind(t,maxRmidS,0.015)
midSplotdat = rbind(midSplotdat1,midSplotdat2,midSplotdat3)

midSplotdat = data.frame(midSplotdat)
names(midSplotdat) = c("t","PropInf","TRate")
midSplotdat$TRate = as.factor(midSplotdat$TRate)

midStrajplot = ggplot()+
  geom_line(data = midSplotdat, aes(x = t, y = PropInf,group = TRate,colour = TRate))+
  ylab("Concurrent Infections")+
  xlab("Time")+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 15))

ind = round(runif(1,min = 0.5,max=50.49))
lowRmaxS = sp0tsdat[ind+50,]

ind = round(runif(1,min = 0.5,max=50.49))
modRmaxS = sp0tsdat[ind+250,]

ind = round(runif(1,min = 0.5,max=50.49))
maxRmaxS = sp0tsdat[ind+500,]

t = 1:2000

maxSplotdat1 = cbind(t,lowRmaxS,0.0087)
maxSplotdat2 = cbind(t,modRmaxS,0.0115)
maxSplotdat3 = cbind(t,maxRmaxS,0.015)
maxSplotdat = rbind(maxSplotdat1,maxSplotdat2,maxSplotdat3)

maxSplotdat = data.frame(maxSplotdat)
names(maxSplotdat) = c("t","PropInf","TRate")
maxSplotdat$TRate = as.factor(maxSplotdat$TRate)

maxStrajplot = ggplot()+
  geom_line(data = maxSplotdat, aes(x = t, y = PropInf,group = TRate,colour = TRate))+
  ylab("Concurrent Infections")+
  xlab("Time")+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 15))

ind = round(runif(1,min = 0.5,max=50.49))
lowRhighS = sp01tsdat[ind+50,]

ind = round(runif(1,min = 0.5,max=50.49))
modRhighS = sp01tsdat[ind+250,]

ind = round(runif(1,min = 0.5,max=50.49))
maxRhighS = sp01tsdat[ind+500,]

t = 1:2000

highSplotdat1 = cbind(t,lowRhighS,0.0087)
highSplotdat2 = cbind(t,modRhighS,0.0115)
highSplotdat3 = cbind(t,maxRhighS,0.015)
highSplotdat = rbind(highSplotdat1,highSplotdat2,highSplotdat3)

highSplotdat = data.frame(highSplotdat)
names(highSplotdat) = c("t","PropInf","TRate")
highSplotdat$TRate = as.factor(highSplotdat$TRate)

highStrajplot = ggplot()+
  geom_line(data = highSplotdat, aes(x = t, y = PropInf,group = TRate,colour = TRate))+
  ylab("Concurrent Infections")+
  xlab("Time")+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 15))

###############################################################################################

# Part 2: Plotting a random excursion. It is inspired by some earlier work identifying when excursions start.

# We'll try using sp1tsdat first, the random sets, because they produce the most excursions.
# We'll also use the maximum r value section.

ind = round(runif(1,min = 0.5,max=50.49))
walksubset = sp1tsdat[ind+500,]

# We now have a walk at random. We want to identify the eeq and the sd of infections, then work out when
# excursions start/end, and then pick a random excursion to plot. It should be easy enough.

# Normally I would check for extinction, but that doesn't happen to this set, so we can skip it.
# We also ignore the idea that we start above the eeq.

eeq = mean(walksubset[1501:2000])
thresh = sqrt(var(walksubset[1501:2000])) + eeq

startTs = c()
endTs = c()
walkinds = walksubset >= thresh

for(t in 2:2000){
 if(t == 2000){
   # End time, we ask if it is also a walk endpoint.
   if(walkinds[t] == TRUE && walkinds[t-1] == TRUE){
     endTs = cbind(endTs,t)
   }
 }
  else{
    # The time is not the end time, so this could be a start or end time for a walk.
    if(walkinds[t] == TRUE){
      if(walkinds[t-1]==TRUE){
        if(walkinds[t+1]==FALSE){
          # This is a walk end.
          endTs = cbind(endTs,t)
        }
      }
      else{
        if(walkinds[t+1]==TRUE){
          # This is a walk start.
          startTs = cbind(startTs,t)
        }
      }
    }
  }
}

# Now we have a vector of starts and corresponding ends. We pick a random start time.
rw = round(runif(1,min = 0.5,max=length(startTs)+0.49))

startTime = startTs[rw]
endTime = endTs[rw]

plotstart = startTime - 10
plotend = endTime + 10

x = c(plotstart:plotend)
y = walksubset[x]

pdat = data.frame(x,y)

walkplot = ggplot()+
  geom_line(data = pdat, aes(x=x,y=y))+
  geom_hline(yintercept = eeq, linetype = 3, colour = "red")+
  geom_hline(yintercept = thresh, colour = "blue")+
  geom_vline(xintercept = startTime, linetype = 2, colour = "red")+
  geom_vline(xintercept = endTime, linetype = 2, colour = "red")+
  xlab("Time")+
  ylab("Concurrent Infections")+
  theme_classic()+
  theme(axis.title  = element_text(size = 20), axis.text = element_text(size = 15))
