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

# We want to extract specific sets of sims from each one to compile into a new set.

maxRset = sp0tsdat[101:110,]
maxRset = rbind(maxRset,sp01tsdat[101:110,])
maxRset = rbind(maxRset,sp02tsdat[101:110,])
maxRset = rbind(maxRset,sp03tsdat[101:110,])
maxRset = rbind(maxRset,sp04tsdat[101:110,])
maxRset = rbind(maxRset,sp05tsdat[101:110,])
maxRset = rbind(maxRset,sp06tsdat[101:110,])
maxRset = rbind(maxRset,sp07tsdat[101:110,])
maxRset = rbind(maxRset,sp08tsdat[101:110,])
maxRset = rbind(maxRset,sp09tsdat[101:110,])
maxRset = rbind(maxRset,sp1tsdat[101:110,])

procdat = c()
tVec = 1:2000

for(i in 1:110){
  row = maxRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    nWalks = length(startTs) # The number of starts/ends is the number of walks.
    walkDur = endTs - startTs # Walk durations.
    aveDur = mean(walkDur) # Average walk duration.
    # Next we want to figure out the average distance walked by a walk.
    peaks = c()
    for(k in 1:length(startTs)){
      walk = row[startTs[k]:endTs[k]]
      peak = max(walk)-eeq
      peaks = cbind(peaks,peak)
    }
    aveDist = mean(peaks)
    procEnt = c(nWalks,aveDur,aveDist,ceiling(i/10))
    procdat = rbind(procdat,procEnt)
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/10))
    procdat = rbind(procdat,procEnt)
  }
}

# Time to try plotting some boxplots, maybe?
procdat = data.frame(procdat)
names(procdat) = c("nWalks","AveDur","AveDist","Group")
procdat$Group = as.factor(procdat$Group)

maxRnwalks <- ggplot()+
  geom_boxplot(dat = procdat, aes(x = Group, y = nWalks))

maxRavedur <- ggplot()+
  geom_boxplot(dat = procdat, aes(x = Group, y = AveDur))

maxRavedist <- ggplot()+
  geom_boxplot(dat = procdat, aes(x = Group, y = AveDist))

# Minimum r value now.
minRset = sp0tsdat[1:10,]
minRset = rbind(minRset,sp01tsdat[1:10,])
minRset = rbind(minRset,sp02tsdat[1:10,])
minRset = rbind(minRset,sp03tsdat[1:10,])
minRset = rbind(minRset,sp04tsdat[1:10,])
minRset = rbind(minRset,sp05tsdat[1:10,])
minRset = rbind(minRset,sp06tsdat[1:10,])
minRset = rbind(minRset,sp07tsdat[1:10,])
minRset = rbind(minRset,sp08tsdat[1:10,])
minRset = rbind(minRset,sp09tsdat[1:10,])
minRset = rbind(minRset,sp1tsdat[1:10,])

procdat2 = c()
tVec = 1:2000

for(i in 1:110){
  row = minRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    nWalks = length(startTs) # The number of starts/ends is the number of walks.
    walkDur = endTs - startTs # Walk durations.
    aveDur = mean(walkDur) # Average walk duration.
    # Next we want to figure out the average distance walked by a walk.
    peaks = c()
    for(k in 1:length(startTs)){
      walk = row[startTs[k]:endTs[k]]
      peak = max(walk)-eeq
      peaks = cbind(peaks,peak)
    }
    aveDist = mean(peaks)
    procEnt = c(nWalks,aveDur,aveDist,ceiling(i/10))
    procdat2 = rbind(procdat2,procEnt)
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/10))
    procdat2 = rbind(procdat2,procEnt)
  }
}

# Time to try plotting some boxplots, maybe?
procdat2 = data.frame(procdat2)
names(procdat2) = c("nWalks","AveDur","AveDist","Group")
procdat2$Group = as.factor(procdat2$Group)

minRnwalks <- ggplot()+
  geom_boxplot(dat = procdat2, aes(x = Group, y = nWalks))

minRavedur <- ggplot()+
  geom_boxplot(dat = procdat2, aes(x = Group, y = AveDur))

minRavedist <- ggplot()+
  geom_boxplot(dat = procdat2, aes(x = Group, y = AveDist))

# The middle r values might be interesting too.

midRset = sp0tsdat[51:60,]
midRset = rbind(midRset,sp01tsdat[51:60,])
midRset = rbind(midRset,sp02tsdat[51:60,])
midRset = rbind(midRset,sp03tsdat[51:60,])
midRset = rbind(midRset,sp04tsdat[51:60,])
midRset = rbind(midRset,sp05tsdat[51:60,])
midRset = rbind(midRset,sp06tsdat[51:60,])
midRset = rbind(midRset,sp07tsdat[51:60,])
midRset = rbind(midRset,sp08tsdat[51:60,])
midRset = rbind(midRset,sp09tsdat[51:60,])
midRset = rbind(midRset,sp1tsdat[51:60,])

procdat3 = c()
tVec = 1:2000

for(i in 1:110){
  row = midRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    if(length(startTs)>0){
      # We need to make sure that excursions actually happened before we try to calculate things.
      nWalks = length(startTs) # The number of starts/ends is the number of walks.
      walkDur = endTs - startTs # Walk durations.
      aveDur = mean(walkDur) # Average walk duration.
      # Next we want to figure out the average distance walked by a walk.
      peaks = c()
      for(k in 1:length(startTs)){
        walk = row[startTs[k]:endTs[k]]
        peak = max(walk)-eeq
        peaks = cbind(peaks,peak)
      }
      aveDist = mean(peaks)
      procEnt = c(nWalks,aveDur,aveDist,ceiling(i/10))
      procdat3 = rbind(procdat3,procEnt)
    }
    else{
      # Otherwise, much like the extinct sims, we use a zero entry.
      procEnt = c(0,0,0,ceiling(i/10))
      procdat3 = rbind(procdat3,procEnt)
    }
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/10))
    procdat3 = rbind(procdat3,procEnt)
  }
}

# Time to try plotting some boxplots, maybe?
procdat3 = data.frame(procdat3)
names(procdat3) = c("nWalks","AveDur","AveDist","Group")
procdat3$Group = as.factor(procdat3$Group)

midRnwalks <- ggplot()+
  geom_boxplot(dat = procdat3, aes(x = Group, y = nWalks))

midRavedur <- ggplot()+
  geom_boxplot(dat = procdat3, aes(x = Group, y = AveDur))

midRavedist <- ggplot()+
  geom_boxplot(dat = procdat3, aes(x = Group, y = AveDist))

#######################################################################################################################

# Now that we have plotted some preliminary boxplots, we will try using actual violin plots to tell a story.
# I'm thinking it would be wise to restrict ourselves to the two extremes and one intermediate, and then
# relegate the rest to the supplementaries IF they are needed (but mostly they will be just to prove we aren't missing
# any weirdness).

#######################################################################################################################

# We want to extract specific sets of sims from each one to compile into a new set.

maxRset = sp0tsdat[501:550,]
maxRset = rbind(maxRset,sp01tsdat[501:550,])
maxRset = rbind(maxRset,sp02tsdat[501:550,])
maxRset = rbind(maxRset,sp03tsdat[501:550,])
maxRset = rbind(maxRset,sp04tsdat[501:550,])
maxRset = rbind(maxRset,sp05tsdat[501:550,])
maxRset = rbind(maxRset,sp06tsdat[501:550,])
maxRset = rbind(maxRset,sp07tsdat[501:550,])
maxRset = rbind(maxRset,sp08tsdat[501:550,])
maxRset = rbind(maxRset,sp09tsdat[501:550,])
maxRset = rbind(maxRset,sp1tsdat[501:550,])

procdat = c()
tVec = 1:2000

for(i in 1:550){
  row = maxRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    if(length(startTs)>0){
      # We need to make sure that excursions actually happened before we try to calculate things.
      nWalks = length(startTs) # The number of starts/ends is the number of walks.
      walkDur = endTs - startTs # Walk durations.
      aveDur = mean(walkDur) # Average walk duration.
      # Next we want to figure out the average distance walked by a walk.
      peaks = c()
      for(k in 1:length(startTs)){
        walk = row[startTs[k]:endTs[k]]
        peak = max(walk)-eeq
        peaks = cbind(peaks,peak)
      }
      aveDist = mean(peaks)
      procEnt = c(nWalks,aveDur,aveDist,ceiling(i/50))
      procdat = rbind(procdat,procEnt)
    }
    else{
      # Otherwise, much like the extinct sims, we use a zero entry.
      procEnt = c(0,0,0,ceiling(i/50))
      procdat = rbind(procdat,procEnt)
    }
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/50))
    procdat = rbind(procdat,procEnt)
  }
}

procdat = data.frame(procdat)
names(procdat) = c("nExcursions","AveDur","AveDist","Group")
procdat$Group = procdat$Group - 1
procdat$Group = 1 - procdat$Group/10
procdat$Group = as.factor(procdat$Group)

maxRexcursions <- ggplot()+
  geom_violin(dat = procdat, aes(x = Group, y = nExcursions))+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,60)+ # Originally 0,100, but shrunk for newer versions.
  theme_classic()

maxRavedur <- ggplot()+
  geom_violin(dat = procdat, aes(x = Group, y = AveDur))+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

maxRavedist <- ggplot()+
  geom_violin(dat = procdat, aes(x = Group, y = AveDist))+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

# We now want to plot the one for the middle r value.

midRset = sp0tsdat[251:300,]
midRset = rbind(midRset,sp01tsdat[251:300,])
midRset = rbind(midRset,sp02tsdat[251:300,])
midRset = rbind(midRset,sp03tsdat[251:300,])
midRset = rbind(midRset,sp04tsdat[251:300,])
midRset = rbind(midRset,sp05tsdat[251:300,])
midRset = rbind(midRset,sp06tsdat[251:300,])
midRset = rbind(midRset,sp07tsdat[251:300,])
midRset = rbind(midRset,sp08tsdat[251:300,])
midRset = rbind(midRset,sp09tsdat[251:300,])
midRset = rbind(midRset,sp1tsdat[251:300,])

procdat2 = c()
tVec = 1:2000

for(i in 1:550){
  row = midRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    if(length(startTs)>0){
      # We need to make sure that excursions actually happened before we try to calculate things.
      nWalks = length(startTs) # The number of starts/ends is the number of walks.
      walkDur = endTs - startTs # Walk durations.
      aveDur = mean(walkDur) # Average walk duration.
      # Next we want to figure out the average distance walked by a walk.
      peaks = c()
      for(k in 1:length(startTs)){
        walk = row[startTs[k]:endTs[k]]
        peak = max(walk)-eeq
        peaks = cbind(peaks,peak)
      }
      aveDist = mean(peaks)
      procEnt = c(nWalks,aveDur,aveDist,ceiling(i/50))
      procdat2 = rbind(procdat2,procEnt)
    }
    else{
      # Otherwise, much like the extinct sims, we use a zero entry.
      procEnt = c(0,0,0,ceiling(i/50))
      procdat2 = rbind(procdat2,procEnt)
    }
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/50))
    procdat2 = rbind(procdat2,procEnt)
  }
}

procdat2 = data.frame(procdat2)
names(procdat2) = c("nExcursions","AveDur","AveDist","Group")
procdat2$Group = procdat2$Group - 1
procdat2$Group = 1 - procdat2$Group/10
procdat2$Group = as.factor(procdat2$Group)

midRexcursions <- ggplot()+
  geom_violin(dat = procdat2, aes(x = Group, y = nExcursions))+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

midRavedur <- ggplot()+
  geom_violin(dat = procdat2, aes(x = Group, y = AveDur))+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

midRavedist <- ggplot()+
  geom_violin(dat = procdat2, aes(x = Group, y = AveDist))+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

# The third plot set is of the minimum r value. This one is the one with the most extinct sims.

minRset = sp0tsdat[1:50,]
minRset = rbind(minRset,sp01tsdat[1:50,])
minRset = rbind(minRset,sp02tsdat[1:50,])
minRset = rbind(minRset,sp03tsdat[1:50,])
minRset = rbind(minRset,sp04tsdat[1:50,])
minRset = rbind(minRset,sp05tsdat[1:50,])
minRset = rbind(minRset,sp06tsdat[1:50,])
minRset = rbind(minRset,sp07tsdat[1:50,])
minRset = rbind(minRset,sp08tsdat[1:50,])
minRset = rbind(minRset,sp09tsdat[1:50,])
minRset = rbind(minRset,sp1tsdat[1:50,])

procdat3 = c()
tVec = 1:2000

for(i in 1:550){
  row = minRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    if(length(startTs)>0){
      # We need to make sure that excursions actually happened before we try to calculate things.
      nWalks = length(startTs) # The number of starts/ends is the number of walks.
      walkDur = endTs - startTs # Walk durations.
      aveDur = mean(walkDur) # Average walk duration.
      # Next we want to figure out the average distance walked by a walk.
      peaks = c()
      for(k in 1:length(startTs)){
        walk = row[startTs[k]:endTs[k]]
        peak = max(walk)-eeq
        peaks = cbind(peaks,peak)
      }
      aveDist = mean(peaks)
      procEnt = c(nWalks,aveDur,aveDist,ceiling(i/50))
      procdat3 = rbind(procdat3,procEnt)
    }
    else{
      # Otherwise, much like the extinct sims, we use a zero entry.
      procEnt = c(0,0,0,ceiling(i/50))
      procdat3 = rbind(procdat3,procEnt)
    }
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/50))
    procdat3 = rbind(procdat3,procEnt)
  }
}

procdat3 = data.frame(procdat3)
names(procdat3) = c("nExcursions","AveDur","AveDist","Group")
procdat3$Group = procdat3$Group - 1
procdat3$Group = 1 - procdat3$Group/10
procdat3$Group = as.factor(procdat3$Group)

minRexcursions <- ggplot()+
  geom_violin(dat = procdat3, aes(x = Group, y = nExcursions))+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

minRavedur <- ggplot()+
  geom_violin(dat = procdat3, aes(x = Group, y = AveDur))+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,150)+
  theme_classic()

minRavedist <- ggplot()+
  geom_violin(dat = procdat3, aes(x = Group, y = AveDist))+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

# A low r value alternative, since the min one tends to go extinct.

lowRset = sp0tsdat[51:100,]
lowRset = rbind(lowRset,sp01tsdat[51:100,])
lowRset = rbind(lowRset,sp02tsdat[51:100,])
lowRset = rbind(lowRset,sp03tsdat[51:100,])
lowRset = rbind(lowRset,sp04tsdat[51:100,])
lowRset = rbind(lowRset,sp05tsdat[51:100,])
lowRset = rbind(lowRset,sp06tsdat[51:100,])
lowRset = rbind(lowRset,sp07tsdat[51:100,])
lowRset = rbind(lowRset,sp08tsdat[51:100,])
lowRset = rbind(lowRset,sp09tsdat[51:100,])
lowRset = rbind(lowRset,sp1tsdat[51:100,])

procdat4 = c()
tVec = 1:2000

for(i in 1:550){
  row = lowRset[i,]
  if(min(row)>0){
    # It did not go extinct, so we can process it.
    eeq = mean(row[1500:2000]) # We find the endemic equilibrium.
    # A short section where we find the s.d. of values in that time frame, and then use mean + sd as a threshold.
    sd = sqrt(var(row[1500:2000]))
    thresh = eeq + sd # If the infections exceed this, we assume a random walk begins.
    threshv2 = eeq - sd # Bonus threshold, for other analysis.
    # Next we find out a load of interesting measures. First attempt was... suboptimal.
    # The first two lines could be useful if we use the lower threshold too, since they prevent some oddities.
    #inds = row>=eeq # Using this to find the first time we reached the eeq.
    #minT = min(tVec[inds]) # First time that the row exceeds the eeq. An acceptable proxy for us reaching it.
    #subrow = row[minT:2000] # We always know that it ends at 2000, thankfully.
    #subTs = minT:2000
    #walkinds = subrow >= thresh # Points where the infections exceed (or equal) the threshold.
    #walkTs = subTs[walkinds] # Times corresponding to those times. We will then identify the start/end times of the walks.
    walkinds = row >= thresh
    startind = 0 # Temp value.
    if(row[1] >= thresh){
      # In this case, we start above the value of the threshold (and it will break the code).
      withins = row <= eeq
      inds = 1:2000
      startind = min(inds[withins]) # Choosing the first index where we reach the eeq.
      # This new startind avoids the initial decrease towards the equilibrium.
    }
    startTs = c()
    endTs = c()
    for(t in max(2,startind):2000){
      # We check to see if a time is a start or end, or a single point. Single points are not walks, I declare.
      if(t == 2000){
        # At the end time, we currently ask if it is an end time.
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
    # Hopefully, the end and start time vectors should be the same length. We can therefore examine them in pairs,
    # with corresponding start/end times representing a single walk.
    if(length(startTs)>0){
      # We need to make sure that excursions actually happened before we try to calculate things.
      nWalks = length(startTs) # The number of starts/ends is the number of walks.
      walkDur = endTs - startTs # Walk durations.
      aveDur = mean(walkDur) # Average walk duration.
      # Next we want to figure out the average distance walked by a walk.
      peaks = c()
      for(k in 1:length(startTs)){
        walk = row[startTs[k]:endTs[k]]
        peak = max(walk)-eeq
        peaks = cbind(peaks,peak)
      }
      aveDist = mean(peaks)
      procEnt = c(nWalks,aveDur,aveDist,ceiling(i/50))
      procdat4 = rbind(procdat4,procEnt)
    }
    else{
      # Otherwise, much like the extinct sims, we use a zero entry.
      procEnt = c(0,0,0,ceiling(i/50))
      procdat4 = rbind(procdat4,procEnt)
    }
  }
  else{
    # This holds the outcome where the row went extinct, and we can't use it.
    procEnt = c(0,0,0,ceiling(i/50))
    procdat4 = rbind(procdat4,procEnt)
  }
}

procdat4 = data.frame(procdat4)
names(procdat4) = c("nExcursions","AveDur","AveDist","Group")
procdat4$Group = procdat4$Group - 1
procdat4$Group = 1 - procdat4$Group/10
procdat4$Group = as.factor(procdat4$Group)

lowRexcursions <- ggplot()+
  geom_violin(dat = procdat4, aes(x = Group, y = nExcursions))+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

lowRavedur <- ggplot()+
  geom_violin(dat = procdat4, aes(x = Group, y = AveDur))+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,150)+
  theme_classic()

lowRavedist <- ggplot()+
  geom_violin(dat = procdat4, aes(x = Group, y = AveDist))+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()

# Having plotted all of those, I'm interested in an equivalent with boxplots overlaid.
maxRexcursionsV2 <- ggplot(dat = procdat, aes(x = Group, y = nExcursions, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,60)+ # Originally 0,100 but shrunk to 0,60 for newer versions.
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

maxRavedurV2 <- ggplot(dat = procdat, aes(x = Group, y = AveDur, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,80)+ # Originally 0,100.
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

maxRavedistV2 <- ggplot(dat = procdat, aes(x = Group, y = AveDist, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,60)+ # Originally... bah, all of them were 0,100 once. It was my standard!
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

midRexcursionsV2 <- ggplot(dat = procdat2, aes(x = Group, y = nExcursions, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,60)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

midRavedurV2 <- ggplot(dat = procdat2, aes(x = Group, y = AveDur, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,100)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

midRavedistV2 <- ggplot(dat = procdat2, aes(x = Group, y = AveDist,fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,80)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

minRexcursionsV2 <- ggplot(dat = procdat3, aes(x = Group, y = nExcursions, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,50)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20))

minRavedurV2 <- ggplot(dat = procdat3, aes(x = Group, y = AveDur, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20))

minRavedistV2 <- ggplot(dat = procdat3, aes(x = Group, y = AveDist, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,50)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20))

lowRexcursionsV2 <- ggplot(dat = procdat4, aes(x = Group, y = nExcursions, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Number of Excursions')+
  xlab('Structuredness')+
  ylim(0,50)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

lowRavedurV2 <- ggplot(dat = procdat4, aes(x = Group, y = AveDur, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Duration')+
  xlab('Structuredness')+
  ylim(0,150)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))

lowRavedistV2 <- ggplot(dat = procdat4, aes(x = Group, y = AveDist, fill = Group))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  ylab('Average Excursion Distance')+
  xlab('Structuredness')+
  ylim(0,80)+
  theme_classic()+
  theme(legend.position = "none", axis.title  = element_text(size = 20), axis.text = element_text(size = 15))


###############################################################################

# For each graph we draw conclusions, and here we can confirm them with the ks tests.
# To do so we must divide the procdat sets into subsets based on group... but I'm using a faster method.

# First, number of excursions for max r sims.
# All others non-significant.

ks.test(procdat$nExcursions[procdat$Group==0],procdat$nExcursions[procdat$Group==1])
ks.test(procdat$nExcursions[procdat$Group==0],procdat$nExcursions[procdat$Group==0.9])
ks.test(procdat$nExcursions[procdat$Group==0],procdat$nExcursions[procdat$Group==0.8])

# Next, the average excursion duration.

ks.test(procdat$AveDur[procdat$Group==0],procdat$AveDur[procdat$Group==1])

# Next, the excursion distance.

ks.test(procdat$AveDist[procdat$Group==0],procdat$AveDist[procdat$Group==1])
ks.test(procdat$AveDist[procdat$Group==0],procdat$AveDist[procdat$Group==0.9])

# Now, we do the mid R simulation data too. Excursions first.

ks.test(procdat2$nExcursions[procdat2$Group==0],procdat2$nExcursions[procdat2$Group==1])
ks.test(procdat2$nExcursions[procdat2$Group==0],procdat2$nExcursions[procdat2$Group==0.9])
ks.test(procdat2$nExcursions[procdat2$Group==0],procdat2$nExcursions[procdat2$Group==0.8])

# Next, average duration.

ks.test(procdat2$AveDur[procdat2$Group==0],procdat2$AveDur[procdat2$Group==1])

# Next, average distance.

ks.test(procdat2$AveDist[procdat2$Group==0],procdat2$AveDist[procdat2$Group==1])
ks.test(procdat2$AveDist[procdat2$Group==0],procdat2$AveDist[procdat2$Group==0.9])

# Finally, we do the low R simulation data. Excursions first, as always.

ks.test(procdat4$nExcursions[procdat4$Group==0],procdat4$nExcursions[procdat4$Group==1])
ks.test(procdat4$nExcursions[procdat4$Group==0],procdat4$nExcursions[procdat4$Group==0.9])

# Next, average duration.

ks.test(procdat4$AveDur[procdat4$Group==0],procdat4$AveDur[procdat4$Group==1])
ks.test(procdat4$AveDur[procdat4$Group==0],procdat4$AveDur[procdat4$Group==0.9])

# Finally, the average distance.

ks.test(procdat4$AveDist[procdat4$Group==0],procdat4$AveDist[procdat4$Group==1])
ks.test(procdat4$AveDist[procdat4$Group==0],procdat4$AveDist[procdat4$Group==0.9])
ks.test(procdat4$AveDist[procdat4$Group==0],procdat4$AveDist[procdat4$Group==0.8])
ks.test(procdat4$AveDist[procdat4$Group==0],procdat4$AveDist[procdat4$Group==0.7])

# The min R stuff is never used, so we will not calculate the p-values here.