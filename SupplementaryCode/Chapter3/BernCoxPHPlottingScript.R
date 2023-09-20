# This is the plotting script - it runs each instance of the coxph function and plots the survival and cumulative hazard graphs
# though does not retain our flexibility since this causes... problems for plotting, particularly with the colours.
# These issues persist regardless, but are significantly worsened by my flexibility code.
library(ggplot2)
library(gridExtra)
library(MASS)
library(plyr)
library(survival)

setwd('~/LeishmaniaPhDStuff/Chapman2018CPWork')

# Loading up some of the data.
CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)

### 1: Only household infection status as a binary covariate.

CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)


for(i in 1:n){
  if(CPDat$Infected[i] == 1){
    # Some infected people are missing symptom onset times, but do have onset years.
    # We need a month, though, so we choose a random month in that year that doesn't violate death/birth months.
    if(is.na(CPDat$SymptomOnset[i]) == TRUE){
      onsetY = CPDat$OnsetYear[i] # Extracting the onset year.
      repMonth = floor((onsetY - 1998)*12 + runif(1,1,12)) # Choosing a month in that year to start off.
      # If only we could stop there. The person must not be infected before birth, or after treatment/death.
      # Not all these apply, so we need to choose a suitable range carefully.
      minPosMn = min(CPDat$BirthMnth[i], 0, na.rm = TRUE) # Minimum possible month.
      maxPosMn = min(CPDat$DiagnosisMnth[i], CPDat$TreatmentMnth[i], CPDat$DeathMnth[i], 78, na.rm = TRUE) # Maximum possible month.
      while(repMonth < minPosMn || repMonth > maxPosMn){
        # Keep rerolling until we end up within the range.
        repMonth = floor((onsetY - 1998)*12 + runif(1,1,12))
      }
      # Update the symptom onset with our placeholder.
      CPDat$SymptomOnset = replace(CPDat$SymptomOnset,i,repMonth)
    }
    # Some people have negative birth months, which we want to cleave for neatness.
    if(is.na(CPDat$BirthMnth[i]) == FALSE){
      if(CPDat$BirthMnth[i] < 0){
        CPDat$BirthMnth = replace(CPDat$BirthMnth, i, NA)
      }
    }
    # Prior infections will end up considered as susceptibles. We want them infected for when we calculate the time
    # that each household became infected.
    if(is.na(CPDat$InfectedPrior[i]) == FALSE){
      CPDat$Infected[i] = 1
      CPDat$SymptomOnset[i] = 1
    }
    # It should be noted that we will remove them again after the household infection times are decided.
    # After all, though they are important to the covariate, they aren't part of the study.
  }
}

# That was just replicating the corrections, we reset CPDat in case you've already run part 1 of this code.
# The main difference is the loop determining when a household was infection. It will be less efficient,
# well slower at least, but more flexible: theoretically it could also serve to do part 1 if the radius is set to 0.

# Loop finding the minimum time each household became infected.
houseinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 1) # subset of people within our chosen distance of individual i.
  #iDist = subset(iDist, iDist > 1) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  houseinf[i] = min(datai, na.rm = TRUE)
}
houseinf[houseinf==Inf]<-NA

for(i in 1:n){
  if(is.na(CPDat$InfectedPrior[i]) == FALSE){
    CPDat$Infected[i] = 0
    CPDat$SymptomOnset[i] = NA
  }
}

# Extracting the prior infections.
preInfSet = subset(CPDat, is.na(CPDat$InfectedPrior) == FALSE)
remInds = preInfSet$Individual # This is a set of indices of individuals to remove from the data.
CPDatCor = CPDat[-remInds,] # Both from the CPDat...
houseinf = houseinf[-remInds] # ... and we remove the corresponding household infection times.

# Deciding on start times: either 0 or the birth time.
# NOTE: I think some people have negative birth times, this will hurt the Cox PH potentially.
starts<-CPDatCor$BirthMnth
starts[is.na(starts)]<-0

# Deciding on stop times: either symptom onset, death, or study end, whichever is first.
# NOTE: Some people are missing symptom onset data, causing them to stop at the death time, not the symptom onset time.
# People also might be death/symptom onset time-less and thus run to the study end, which is incorrect.
stops<-apply(cbind(CPDatCor$DeathMnth,CPDatCor$SymptomOnset,76),1,min,na.rm=T)

# If the stop time is equal to the symptom onset, we record an event.
# Ah. Not a good thing, given the missing symptom onset times - corrected for earlier.
events<-stops==CPDatCor$SymptomOnset
events[is.na(events)]<-0

# Curating our data. Add things like closeinf if they are present.
data1<-data.frame(INDID=CPDatCor$IID,starts,stops,events,houseinf)
data1[1:16,]

# Replicating the stop data.
data2<-tmerge(data1,data1,id=INDID,tstop=stops)
data2[1:16,]

# Use tmerge to split windows when someone in household gets infected. The result is an enlarged matrix of data. Rows refer to observation windows. The field hsympt is one for windows in which someone else in household is symptomatic
data2<-tmerge(data2,data2,id=INDID,hsympt=tdc(houseinf))
data2[1:16,]

cph01<-coxph(Surv(tstart,tstop,events)~hsympt,data=data2)
cph01

s01<-survfit(cph01,newdata=data2)
s01MP<-survfit(Surv(tstart,tstop,events)~hsympt,data=data2)
# Household infection as only covariate.
# ... and it broke again... the madness is that it's plotting with colour 1 and 3 instead of 1 and 2...
# Until I figure out why this is even able to happen, we'll use 2 as a dummy colour. It's likely we'll need to use this
# workaround on other plots too, but I won't be able to predict them in advance.
plot(s01,conf.int=T,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0),rgb(1.0,0.0,0.0)),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
legend("bottomleft",legend=c("Uninfected Household","Infected Household"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))
plot(s01,cumhaz=T,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0),rgb(1.0,0.0,0.0)),xlim=c(0,80), ylim = c(0.0, 0.5), xlab="Time (months)", ylab="Cumulative Hazard")
legend("topleft",legend=c("Uninfected Household","Infected Household"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))

### 2: Infection between 1 and 25m as only covariate.

CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)

for(i in 1:n){
  if(CPDat$Infected[i] == 1){
    # Some infected people are missing symptom onset times, but do have onset years.
    # We need a month, though, so we choose a random month in that year that doesn't violate death/birth months.
    if(is.na(CPDat$SymptomOnset[i]) == TRUE){
      onsetY = CPDat$OnsetYear[i] # Extracting the onset year.
      repMonth = floor((onsetY - 1998)*12 + runif(1,1,12)) # Choosing a month in that year to start off.
      # If only we could stop there. The person must not be infected before birth, or after treatment/death.
      # Not all these apply, so we need to choose a suitable range carefully.
      minPosMn = min(CPDat$BirthMnth[i], 0, na.rm = TRUE) # Minimum possible month.
      maxPosMn = min(CPDat$DiagnosisMnth[i], CPDat$TreatmentMnth[i], CPDat$DeathMnth[i], 78, na.rm = TRUE) # Maximum possible month.
      while(repMonth < minPosMn || repMonth > maxPosMn){
        # Keep rerolling until we end up within the range.
        repMonth = floor((onsetY - 1998)*12 + runif(1,1,12))
      }
      # Update the symptom onset with our placeholder.
      CPDat$SymptomOnset = replace(CPDat$SymptomOnset,i,repMonth)
    }
    # Some people have negative birth months, which we want to cleave for neatness.
    if(is.na(CPDat$BirthMnth[i]) == FALSE){
      if(CPDat$BirthMnth[i] < 0){
        CPDat$BirthMnth = replace(CPDat$BirthMnth, i, NA)
      }
    }
    # Prior infections will end up considered as susceptibles. We want them infected for when we calculate the time
    # that each household became infected.
    if(is.na(CPDat$InfectedPrior[i]) == FALSE){
      CPDat$Infected[i] = 1
      CPDat$SymptomOnset[i] = 1
    }
    # It should be noted that we will remove them again after the household infection times are decided.
    # After all, though they are important to the covariate, they aren't part of the study.
  }
}

# That was just replicating the corrections, we reset CPDat in case you've already run part 1 of this code.
# The main difference is the loop determining when a household was infection. It will be less efficient,
# well slower at least, but more flexible: theoretically it could also serve to do part 1 if the radius is set to 0.

# Loop finding the minimum time each household became infected.

closeinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 25) # subset of people within our chosen distance of individual i.
  iDist = subset(iDist, iDist >= 1) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  closeinf[i] = min(datai, na.rm = TRUE)
}
closeinf[closeinf==Inf]<-NA

for(i in 1:n){
  if(is.na(CPDat$InfectedPrior[i]) == FALSE){
    CPDat$Infected[i] = 0
    CPDat$SymptomOnset[i] = NA
  }
}

# Extracting the prior infections.
preInfSet = subset(CPDat, is.na(CPDat$InfectedPrior) == FALSE)
remInds = preInfSet$Individual # This is a set of indices of individuals to remove from the data.
CPDatCor = CPDat[-remInds,] # Both from the CPDat...
closeinf = closeinf[-remInds] # ... and also the close infection times too.

# Deciding on start times: either 0 or the birth time.
# NOTE: I think some people have negative birth times, this will hurt the Cox PH potentially.
starts<-CPDatCor$BirthMnth
starts[is.na(starts)]<-0

# Deciding on stop times: either symptom onset, death, or study end, whichever is first.
# NOTE: Some people are missing symptom onset data, causing them to stop at the death time, not the symptom onset time.
# People also might be death/symptom onset time-less and thus run to the study end, which is incorrect.
stops<-apply(cbind(CPDatCor$DeathMnth,CPDatCor$SymptomOnset,76),1,min,na.rm=T)

# If the stop time is equal to the symptom onset, we record an event.
# Ah. Not a good thing, given the missing symptom onset times - corrected for earlier.
events<-stops==CPDatCor$SymptomOnset
events[is.na(events)]<-0

# Curating our data. Add things like closeinf if they are present.
data1<-data.frame(INDID=CPDatCor$IID,starts,stops,events,closeinf)
data1[1:16,]

# Replicating the stop data.
data2<-tmerge(data1,data1,id=INDID,tstop=stops)
data2[1:16,]

# Use tmerge to split windows when someone in household gets infected. The result is an enlarged matrix of data. Rows refer to observation windows. The field hsympt is one for windows in which someone else in household is symptomatic
data2<-tmerge(data2,data2,id=INDID,csympt=tdc(closeinf))
data2[1:16,]

cph02<-coxph(Surv(tstart,tstop,events)~csympt,data=data2)
cph02

s02<-survfit(cph02,newdata=data2)
s02MP<-survfit(Surv(tstart,tstop,events)~csympt,data=data2)
plot(s02,conf.int=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
legend("bottomleft",legend=c("No Infection in 1-25m Band","Infection in 1-25m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))
plot(s02,cumhaz=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.0, 0.5), xlab="Time (months)", ylab="Cumulative Hazard")
legend("topleft",legend=c("No Infection in 1-25m Band","Infection in 1-25m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))

# 3: Infection between 25 and 50m away as only covariate.

CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)

for(i in 1:n){
  if(CPDat$Infected[i] == 1){
    # Some infected people are missing symptom onset times, but do have onset years.
    # We need a month, though, so we choose a random month in that year that doesn't violate death/birth months.
    if(is.na(CPDat$SymptomOnset[i]) == TRUE){
      onsetY = CPDat$OnsetYear[i] # Extracting the onset year.
      repMonth = floor((onsetY - 1998)*12 + runif(1,1,12)) # Choosing a month in that year to start off.
      # If only we could stop there. The person must not be infected before birth, or after treatment/death.
      # Not all these apply, so we need to choose a suitable range carefully.
      minPosMn = min(CPDat$BirthMnth[i], 0, na.rm = TRUE) # Minimum possible month.
      maxPosMn = min(CPDat$DiagnosisMnth[i], CPDat$TreatmentMnth[i], CPDat$DeathMnth[i], 78, na.rm = TRUE) # Maximum possible month.
      while(repMonth < minPosMn || repMonth > maxPosMn){
        # Keep rerolling until we end up within the range.
        repMonth = floor((onsetY - 1998)*12 + runif(1,1,12))
      }
      # Update the symptom onset with our placeholder.
      CPDat$SymptomOnset = replace(CPDat$SymptomOnset,i,repMonth)
    }
    # Some people have negative birth months, which we want to cleave for neatness.
    if(is.na(CPDat$BirthMnth[i]) == FALSE){
      if(CPDat$BirthMnth[i] < 0){
        CPDat$BirthMnth = replace(CPDat$BirthMnth, i, NA)
      }
    }
    # Prior infections will end up considered as susceptibles. We want them infected for when we calculate the time
    # that each household became infected.
    if(is.na(CPDat$InfectedPrior[i]) == FALSE){
      CPDat$Infected[i] = 1
      CPDat$SymptomOnset[i] = 1
    }
    # It should be noted that we will remove them again after the household infection times are decided.
    # After all, though they are important to the covariate, they aren't part of the study.
  }
}

# That was just replicating the corrections, we reset CPDat in case you've already run part 1 of this code.
# The main difference is the loop determining when a household was infection. It will be less efficient,
# well slower at least, but more flexible: theoretically it could also serve to do part 1 if the radius is set to 0.

distinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 50) # subset of people within our chosen distance of individual i.
  iDist = subset(iDist, iDist >= 25) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  distinf[i] = min(datai, na.rm = TRUE)
}
distinf[distinf==Inf]<-NA

for(i in 1:n){
  if(is.na(CPDat$InfectedPrior[i]) == FALSE){
    CPDat$Infected[i] = 0
    CPDat$SymptomOnset[i] = NA
  }
}

# Extracting the prior infections.
preInfSet = subset(CPDat, is.na(CPDat$InfectedPrior) == FALSE)
remInds = preInfSet$Individual # This is a set of indices of individuals to remove from the data.
CPDatCor = CPDat[-remInds,] # Both from the CPDat...
distinf = distinf[-remInds] # ... and also the dist infection times too.
# Deciding on start times: either 0 or the birth time.
# NOTE: I think some people have negative birth times, this will hurt the Cox PH potentially.
starts<-CPDatCor$BirthMnth
starts[is.na(starts)]<-0

# Deciding on stop times: either symptom onset, death, or study end, whichever is first.
# NOTE: Some people are missing symptom onset data, causing them to stop at the death time, not the symptom onset time.
# People also might be death/symptom onset time-less and thus run to the study end, which is incorrect.
stops<-apply(cbind(CPDatCor$DeathMnth,CPDatCor$SymptomOnset,76),1,min,na.rm=T)

# If the stop time is equal to the symptom onset, we record an event.
# Ah. Not a good thing, given the missing symptom onset times - corrected for earlier.
events<-stops==CPDatCor$SymptomOnset
events[is.na(events)]<-0

# Curating our data. Add things like closeinf if they are present.
data1<-data.frame(INDID=CPDatCor$IID,starts,stops,events,houseinf,closeinf,distinf)
data1[1:16,]

# Replicating the stop data.
data2<-tmerge(data1,data1,id=INDID,tstop=stops)
data2[1:16,]

# Use tmerge to split windows when someone in household gets infected. The result is an enlarged matrix of data. Rows refer to observation windows. The field hsympt is one for windows in which someone else in household is symptomatic
data2<-tmerge(data2,data2,id=INDID,dsympt=tdc(distinf))
data2[1:16,]

cph03<-coxph(Surv(tstart,tstop,events)~dsympt,data=data2)
cph03

s03<-survfit(cph03,newdata=data2)
s03MP<-survfit(Surv(tstart,tstop,events)~dsympt,data=data2)
plot(s03,conf.int=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
legend("bottomleft",legend=c("No Infection in 25-50m Band","Infection in 25-50m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))
plot(s03,cumhaz=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.0, 0.5), xlab="Time (months)", ylab="Cumulative Hazard")
legend("topleft",legend=c("No Infection in 25-50m Band","Infection in 25-50m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))

# 4: A reference plot; infection between 50 and 75m away.

CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)

for(i in 1:n){
  if(CPDat$Infected[i] == 1){
    # Some infected people are missing symptom onset times, but do have onset years.
    # We need a month, though, so we choose a random month in that year that doesn't violate death/birth months.
    if(is.na(CPDat$SymptomOnset[i]) == TRUE){
      onsetY = CPDat$OnsetYear[i] # Extracting the onset year.
      repMonth = floor((onsetY - 1998)*12 + runif(1,1,12)) # Choosing a month in that year to start off.
      # If only we could stop there. The person must not be infected before birth, or after treatment/death.
      # Not all these apply, so we need to choose a suitable range carefully.
      minPosMn = min(CPDat$BirthMnth[i], 0, na.rm = TRUE) # Minimum possible month.
      maxPosMn = min(CPDat$DiagnosisMnth[i], CPDat$TreatmentMnth[i], CPDat$DeathMnth[i], 78, na.rm = TRUE) # Maximum possible month.
      while(repMonth < minPosMn || repMonth > maxPosMn){
        # Keep rerolling until we end up within the range.
        repMonth = floor((onsetY - 1998)*12 + runif(1,1,12))
      }
      # Update the symptom onset with our placeholder.
      CPDat$SymptomOnset = replace(CPDat$SymptomOnset,i,repMonth)
    }
    # Some people have negative birth months, which we want to cleave for neatness.
    if(is.na(CPDat$BirthMnth[i]) == FALSE){
      if(CPDat$BirthMnth[i] < 0){
        CPDat$BirthMnth = replace(CPDat$BirthMnth, i, NA)
      }
    }
    # Prior infections will end up considered as susceptibles. We want them infected for when we calculate the time
    # that each household became infected.
    if(is.na(CPDat$InfectedPrior[i]) == FALSE){
      CPDat$Infected[i] = 1
      CPDat$SymptomOnset[i] = 1
    }
    # It should be noted that we will remove them again after the household infection times are decided.
    # After all, though they are important to the covariate, they aren't part of the study.
  }
}

# That was just replicating the corrections, we reset CPDat in case you've already run part 1 of this code.
# The main difference is the loop determining when a household was infection. It will be less efficient,
# well slower at least, but more flexible: theoretically it could also serve to do part 1 if the radius is set to 0.

distinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 75) # subset of people within our chosen distance of individual i.
  iDist = subset(iDist, iDist >= 50) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  distinf[i] = min(datai, na.rm = TRUE)
}
distinf[distinf==Inf]<-NA

for(i in 1:n){
  if(is.na(CPDat$InfectedPrior[i]) == FALSE){
    CPDat$Infected[i] = 0
    CPDat$SymptomOnset[i] = NA
  }
}

# Extracting the prior infections.
preInfSet = subset(CPDat, is.na(CPDat$InfectedPrior) == FALSE)
remInds = preInfSet$Individual # This is a set of indices of individuals to remove from the data.
CPDatCor = CPDat[-remInds,] # Both from the CPDat...
distinf = distinf[-remInds] # ... and also the dist infection times too.
# Deciding on start times: either 0 or the birth time.
# NOTE: I think some people have negative birth times, this will hurt the Cox PH potentially.
starts<-CPDatCor$BirthMnth
starts[is.na(starts)]<-0

# Deciding on stop times: either symptom onset, death, or study end, whichever is first.
# NOTE: Some people are missing symptom onset data, causing them to stop at the death time, not the symptom onset time.
# People also might be death/symptom onset time-less and thus run to the study end, which is incorrect.
stops<-apply(cbind(CPDatCor$DeathMnth,CPDatCor$SymptomOnset,76),1,min,na.rm=T)

# If the stop time is equal to the symptom onset, we record an event.
# Ah. Not a good thing, given the missing symptom onset times - corrected for earlier.
events<-stops==CPDatCor$SymptomOnset
events[is.na(events)]<-0

# Curating our data. Add things like closeinf if they are present.
data1<-data.frame(INDID=CPDatCor$IID,starts,stops,events,houseinf,closeinf,distinf)
data1[1:16,]

# Replicating the stop data.
data2<-tmerge(data1,data1,id=INDID,tstop=stops)
data2[1:16,]

# Use tmerge to split windows when someone in household gets infected. The result is an enlarged matrix of data. Rows refer to observation windows. The field hsympt is one for windows in which someone else in household is symptomatic
data2<-tmerge(data2,data2,id=INDID,dsympt=tdc(distinf))
data2[1:16,]

cph04<-coxph(Surv(tstart,tstop,events)~dsympt,data=data2)
cph04

s04<-survfit(cph04,newdata=data2)
s04MP<-survfit(Surv(tstart,tstop,events)~dsympt,data=data2)
plot(s04,conf.int=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
legend("bottomleft",legend=c("No Infection in 50-75m Band","Infection in 50-75m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))
plot(s04,cumhaz=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.0, 0.5), xlab="Time (months)", ylab="Cumulative Hazard")
legend("topleft",legend=c("No Infection in 50-75m Band","Infection in 50-75m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))

# I'm hoping to combine these into a grid of 4.
# Unlike ggplot, however, plot doesn't seem to save properly to combine into a grid.
# It looks like I can't get plot to become a grob to arrange, so this is going on ice for now. Onwards, then:

### 5: Fitting one model with the first 3 covariates + interaction terms.


# Loading up some of the data.
CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)


for(i in 1:n){
  if(CPDat$Infected[i] == 1){
    # Some infected people are missing symptom onset times, but do have onset years.
    # We need a month, though, so we choose a random month in that year that doesn't violate death/birth months.
    if(is.na(CPDat$SymptomOnset[i]) == TRUE){
      onsetY = CPDat$OnsetYear[i] # Extracting the onset year.
      repMonth = floor((onsetY - 1998)*12 + runif(1,1,12)) # Choosing a month in that year to start off.
      # If only we could stop there. The person must not be infected before birth, or after treatment/death.
      # Not all these apply, so we need to choose a suitable range carefully.
      minPosMn = min(CPDat$BirthMnth[i], 0, na.rm = TRUE) # Minimum possible month.
      maxPosMn = min(CPDat$DiagnosisMnth[i], CPDat$TreatmentMnth[i], CPDat$DeathMnth[i], 78, na.rm = TRUE) # Maximum possible month.
      while(repMonth < minPosMn || repMonth > maxPosMn){
        # Keep rerolling until we end up within the range.
        repMonth = floor((onsetY - 1998)*12 + runif(1,1,12))
      }
      # Update the symptom onset with our placeholder.
      CPDat$SymptomOnset = replace(CPDat$SymptomOnset,i,repMonth)
    }
    # Some people have negative birth months, which we want to cleave for neatness.
    if(is.na(CPDat$BirthMnth[i]) == FALSE){
      if(CPDat$BirthMnth[i] < 0){
        CPDat$BirthMnth = replace(CPDat$BirthMnth, i, NA)
      }
    }
    # Prior infections will end up considered as susceptibles. We want them infected for when we calculate the time
    # that each household became infected.
    if(is.na(CPDat$InfectedPrior[i]) == FALSE){
      CPDat$Infected[i] = 1
      CPDat$SymptomOnset[i] = 1
    }
    # It should be noted that we will remove them again after the household infection times are decided.
    # After all, though they are important to the covariate, they aren't part of the study.
  }
}

# That was just replicating the corrections, we reset CPDat in case you've already run part 1 of this code.
# The main difference is the loop determining when a household was infection. It will be less efficient,
# well slower at least, but more flexible: theoretically it could also serve to do part 1 if the radius is set to 0.

# Loop finding the minimum time each household became infected.
houseinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 1) # subset of people within our chosen distance of individual i.
  #iDist = subset(iDist, iDist > 1) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  houseinf[i] = min(datai, na.rm = TRUE)
}
houseinf[houseinf==Inf]<-NA

closeinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 25) # subset of people within our chosen distance of individual i.
  iDist = subset(iDist, iDist >= 1) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  closeinf[i] = min(datai, na.rm = TRUE)
}
closeinf[closeinf==Inf]<-NA

distinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 50) # subset of people within our chosen distance of individual i.
  iDist = subset(iDist, iDist >= 25) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  distinf[i] = min(datai, na.rm = TRUE)
}
distinf[distinf==Inf]<-NA

for(i in 1:n){
  if(is.na(CPDat$InfectedPrior[i]) == FALSE){
    CPDat$Infected[i] = 0
    CPDat$SymptomOnset[i] = NA
  }
}

# Extracting the prior infections.
preInfSet = subset(CPDat, is.na(CPDat$InfectedPrior) == FALSE)
remInds = preInfSet$Individual # This is a set of indices of individuals to remove from the data.
CPDatCor = CPDat[-remInds,] # Both from the CPDat...
houseinf = houseinf[-remInds] # ... and we remove the corresponding household infection times.
closeinf = closeinf[-remInds] # ... and also the close infection times too.
distinf = distinf[-remInds] # ... and also the dist infection times too.
#distinf2 = distinf2[-remInds] # ... and also the very dist infection times too.
#distinf3 = distinf3[-remInds] # ... and also the very dist infection times too.

# Deciding on start times: either 0 or the birth time.
# NOTE: I think some people have negative birth times, this will hurt the Cox PH potentially.
starts<-CPDatCor$BirthMnth
starts[is.na(starts)]<-0

# Deciding on stop times: either symptom onset, death, or study end, whichever is first.
# NOTE: Some people are missing symptom onset data, causing them to stop at the death time, not the symptom onset time.
# People also might be death/symptom onset time-less and thus run to the study end, which is incorrect.
stops<-apply(cbind(CPDatCor$DeathMnth,CPDatCor$SymptomOnset,76),1,min,na.rm=T)

# If the stop time is equal to the symptom onset, we record an event.
# Ah. Not a good thing, given the missing symptom onset times - corrected for earlier.
events<-stops==CPDatCor$SymptomOnset
events[is.na(events)]<-0

# Curating our data. Add things like closeinf if they are present.
data1<-data.frame(INDID=CPDatCor$IID,starts,stops,events,houseinf,closeinf,distinf)
data1[1:16,]

# Replicating the stop data.
data2<-tmerge(data1,data1,id=INDID,tstop=stops)
data2[1:16,]

# Use tmerge to split windows when someone in household gets infected. The result is an enlarged matrix of data. Rows refer to observation windows. The field hsympt is one for windows in which someone else in household is symptomatic
data2<-tmerge(data2,data2,id=INDID,hsympt=tdc(houseinf),csympt=tdc(closeinf),dsympt=tdc(distinf))
data2[1:16,]

cph05<-coxph(Surv(tstart,tstop,events)~hsympt*csympt*dsympt,data=data2)
cph05

s05<-survfit(cph05,newdata=data2)
plot(s05,conf.int=T,col=c(1,2,3,4,5,6,7),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
legend("bottomleft",legend=c("No Infection in 50-75m Band","Infection in 50-75m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))
plot(s05,cumhaz=T,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.0, 0.5), xlab="Time (months)", ylab="Cumulative Hazard")
legend("topleft",legend=c("No Infection in 50-75m Band","Infection in 50-75m Band"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))

### 6: Comparing the time-delayed version with the standard one - two plots, but both connected.

CPDat <- read.csv('S1Data-ModelFittingDataAnnotated.csv', header = TRUE)
distMat <- read.csv('S2_Data.csv', header = FALSE) # ... and the distance matrix.
distMat = as.matrix(distMat) # I want this as a matrix, for a change.

Individual = c(1:2494)
CPDat = cbind(CPDat, Individual)

# Some diagnostic info - used in loops later.
n<-nrow(CPDat)


for(i in 1:n){
  if(CPDat$Infected[i] == 1){
    # Some infected people are missing symptom onset times, but do have onset years.
    # We need a month, though, so we choose a random month in that year that doesn't violate death/birth months.
    if(is.na(CPDat$SymptomOnset[i]) == TRUE){
      onsetY = CPDat$OnsetYear[i] # Extracting the onset year.
      repMonth = floor((onsetY - 1998)*12 + runif(1,1,12)) # Choosing a month in that year to start off.
      # If only we could stop there. The person must not be infected before birth, or after treatment/death.
      # Not all these apply, so we need to choose a suitable range carefully.
      minPosMn = min(CPDat$BirthMnth[i], 0, na.rm = TRUE) # Minimum possible month.
      maxPosMn = min(CPDat$DiagnosisMnth[i], CPDat$TreatmentMnth[i], CPDat$DeathMnth[i], 78, na.rm = TRUE) # Maximum possible month.
      while(repMonth < minPosMn || repMonth > maxPosMn){
        # Keep rerolling until we end up within the range.
        repMonth = floor((onsetY - 1998)*12 + runif(1,1,12))
      }
      # Update the symptom onset with our placeholder.
      CPDat$SymptomOnset = replace(CPDat$SymptomOnset,i,repMonth)
    }
    # Some people have negative birth months, which we want to cleave for neatness.
    if(is.na(CPDat$BirthMnth[i]) == FALSE){
      if(CPDat$BirthMnth[i] < 0){
        CPDat$BirthMnth = replace(CPDat$BirthMnth, i, NA)
      }
    }
    # Prior infections will end up considered as susceptibles. We want them infected for when we calculate the time
    # that each household became infected.
    if(is.na(CPDat$InfectedPrior[i]) == FALSE){
      CPDat$Infected[i] = 1
      CPDat$SymptomOnset[i] = 1
    }
    # It should be noted that we will remove them again after the household infection times are decided.
    # After all, though they are important to the covariate, they aren't part of the study.
  }
}

# That was just replicating the corrections, we reset CPDat in case you've already run part 1 of this code.
# The main difference is the loop determining when a household was infection. It will be less efficient,
# well slower at least, but more flexible: theoretically it could also serve to do part 1 if the radius is set to 0.

# Loop finding the minimum time each household became infected.
houseinf<-c()
for(i in 1:n){
  # First, we extract the relevant row of the distance matrix.
  iDist = distMat[i,]
  # Then bind the individual row from before to it.
  iDist = cbind(iDist, Individual)
  iDist = data.frame(iDist) # Convenient to have a dataframe for this.
  iDist = iDist[-i,] # Remove individual i, because don't really want to count them!
  iDist = subset(iDist, iDist < 1) # subset of people within our chosen distance of individual i.
  #iDist = subset(iDist, iDist > 1) # Optional line to create a narrow band instead of a circle.
  # We now have a list, iDist$Individual, of people within our desired distance. We filter CPDat using it, and then do the same as
  # before: pick the minimum symptom onset time.
  datai = CPDat$SymptomOnset[iDist$Individual]
  houseinf[i] = min(datai, na.rm = TRUE)
}
houseinf[houseinf==Inf]<-NA

for(i in 1:n){
  if(is.na(CPDat$InfectedPrior[i]) == FALSE){
    CPDat$Infected[i] = 0
    CPDat$SymptomOnset[i] = NA
  }
}

# Extracting the prior infections.
preInfSet = subset(CPDat, is.na(CPDat$InfectedPrior) == FALSE)
remInds = preInfSet$Individual # This is a set of indices of individuals to remove from the data.
CPDatCor = CPDat[-remInds,] # Both from the CPDat...
houseinf = houseinf[-remInds] # ... and we remove the corresponding household infection times.

# Deciding on start times: either 0 or the birth time.
# NOTE: I think some people have negative birth times, this will hurt the Cox PH potentially.
starts<-CPDatCor$BirthMnth
starts[is.na(starts)]<-0

# Deciding on stop times: either symptom onset, death, or study end, whichever is first.
# NOTE: Some people are missing symptom onset data, causing them to stop at the death time, not the symptom onset time.
# People also might be death/symptom onset time-less and thus run to the study end, which is incorrect.
stops<-apply(cbind(CPDatCor$DeathMnth,CPDatCor$SymptomOnset,76),1,min,na.rm=T)

# If the stop time is equal to the symptom onset, we record an event.
# Ah. Not a good thing, given the missing symptom onset times - corrected for earlier.
events<-stops==CPDatCor$SymptomOnset
events[is.na(events)]<-0

# Curating our data. Add things like closeinf if they are present.
data1<-data.frame(INDID=CPDatCor$IID,starts,stops,events,houseinf)
data1[1:16,]

# Replicating the stop data.
data2<-tmerge(data1,data1,id=INDID,tstop=stops)
data2[1:16,]

# Use tmerge to split windows when someone in household gets infected. The result is an enlarged matrix of data. Rows refer to observation windows. The field hsympt is one for windows in which someone else in household is symptomatic
data2<-tmerge(data2,data2,id=INDID,hsympt=tdc(houseinf),options=list(delay=4))
data2[1:16,]

cph06<-coxph(Surv(tstart,tstop,events)~hsympt,data=data2)
cph06

s06<-survfit(cph06,newdata=data2)
# The colour madness reaches new heights in this section.
plot(s06,conf.int=T,col=c(rgb(1.0,0.0,0.0),rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
legend("bottomleft",legend=c("Uninfected Household","Infected Household"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)))
plot(s06,cumhaz=T,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0)),xlim=c(0,80), ylim = c(0.0, 0.5), xlab="Time (months)", ylab="Cumulative Hazard")
legend("topleft",legend=c("Uninfected Household","Infected Household"),pch = 15,col=c(rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)))

# The next is a fancier comparison plot.
# The colours are driving me mad. This first line is plotting with colours 2 and 3, not 1 for some reason.
plot(s06,conf.int=T,col=c(rgb(1.0,0.0,1.0),rgb(1.0,0.0,0.0),rgb(0.0,0.0,1.0)),xlim=c(0,80), ylim = c(0.35, 1.0), xlab="Time (months)", ylab="Survival")
# So... colour 1 and colour 3 still used for this one. Goodness knows how it's picking the colours...
lines(s01,conf.int=T,col=c(rgb(1.0,0.1,1.0),rgb(0.5,0.0,0.5),rgb(0.0,0.0,0.0)))
# Legend time - now that I've figured the colours out.
legend("bottomleft",legend=c("Uninfected Household (Delay)","Infected Household (Delay)","Uninfected Household", "Infected Household"),pch = 15,col=c(rgb(0.0,0.0,1.0),rgb(1.0,0.0,0.0),rgb(1.0,0.1,1.0),rgb(0.0,0.0,0.0)))

# There is another possibility that may be added in here: a spline based fitting of "distance to nearest case" as the
# only covariate.

# Bonus plot time: a multiplot version of plots 1-4, using ggplot and gridExtra.
# We use s01MP, s02MP, s03MP and s04MP

# Each plot needs to be created individually and then assembled into a grid.
s1nuh <- s01MP$strata[1]
s1toth <- length(s01MP$time)
s1uht <- s01MP$time[1:s1nuh]
s1uhm <- s01MP$surv[1:s1nuh]
s1uhuc <- s01MP$upper[1:s1nuh]
s1uhlc <- s01MP$lower[1:s1nuh]
s1iht <- s01MP$time[(s1nuh+1):s1toth]
s1ihm <- s01MP$surv[(s1nuh+1):s1toth]
s1ihuc <- s01MP$upper[(s1nuh+1):s1toth]
s1ihlc <- s01MP$lower[(s1nuh+1):s1toth]
# For the purposes of plotting, we need to add a single initial point for these.
s1uht = c(0,s1uht)
s1uhm = c(1,s1uhm)
s1uhuc = c(1,s1uhuc)
s1uhlc = c(1,s1uhlc)
s1iht = c(0,s1iht)
s1ihm = c(1,s1ihm)
s1ihuc = c(1,s1ihuc)
s1ihlc = c(1,s1ihlc)
# Turns out that figure legends only happen automatically. Time to combine these into 3 sets and then use those instead.
s1uhmCons = data.frame(s1uht,s1uhm,c("Uninfected"))
names(s1uhmCons) = c("Time","Survival","HState")
s1uhucCons = data.frame(s1uht,s1uhuc,c("Uninfected"))
names(s1uhucCons) = c("Time","Survival","HState")
s1uhlcCons = data.frame(s1uht,s1uhlc,c("Uninfected"))
names(s1uhlcCons) = c("Time","Survival","HState")
s1ihmCons = data.frame(s1iht,s1ihm,c("Infected"))
names(s1ihmCons) = c("Time","Survival","HState")
s1ihucCons = data.frame(s1iht,s1ihuc,c("Infected"))
names(s1ihucCons) = c("Time","Survival","HState")
s1ihlcCons = data.frame(s1iht,s1ihlc,c("Infected"))
names(s1ihlcCons) = c("Time","Survival","HState")
s1mdat = rbind(s1uhmCons,s1ihmCons)
s1ucdat = rbind(s1uhucCons,s1ihucCons)
s1lcdat = rbind(s1uhlcCons,s1ihlcCons)
s1mdat$HState = as.factor(s1mdat$HState)
s1ucdat$HState = as.factor(s1ucdat$HState)
s1lcdat$HState = as.factor(s1lcdat$HState)

s01Plot<-ggplot()+
  geom_step(data=s1mdat,aes(x=Time,y=Survival,colour=HState))+
  geom_step(data=s1ucdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  geom_step(data=s1lcdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  annotate("text", x = 5, y = 0.95, label = "A")+
  ylim(0.7,1.0)+
  xlab("Time (Months)")+
  ylab("Survival")+
  labs(colour="Household Infection Status")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        legend.box.background = element_rect(color="black",size=1.2), axis.title.x = element_blank(),
        axis.text.x = element_blank())

# Next, s02.

s2nuh <- s02MP$strata[1]
s2toth <- length(s02MP$time)
s2uht <- s02MP$time[1:s2nuh]
s2uhm <- s02MP$surv[1:s2nuh]
s2uhuc <- s02MP$upper[1:s2nuh]
s2uhlc <- s02MP$lower[1:s2nuh]
s2iht <- s02MP$time[(s2nuh+1):s2toth]
s2ihm <- s02MP$surv[(s2nuh+1):s2toth]
s2ihuc <- s02MP$upper[(s2nuh+1):s2toth]
s2ihlc <- s02MP$lower[(s2nuh+1):s2toth]
# For the purposes of plotting, we need to add a single initial point for these.
s2uht = c(0,s2uht)
s2uhm = c(1,s2uhm)
s2uhuc = c(1,s2uhuc)
s2uhlc = c(1,s2uhlc)
s2iht = c(0,s2iht)
s2ihm = c(1,s2ihm)
s2ihuc = c(1,s2ihuc)
s2ihlc = c(1,s2ihlc)
# Turns out that figure legends only happen automatically. Time to combine these into 3 sets and then use those instead.
s2uhmCons = data.frame(s2uht,s2uhm,c("Uninfected"))
names(s2uhmCons) = c("Time","Survival","HState")
s2uhucCons = data.frame(s2uht,s2uhuc,c("Uninfected"))
names(s2uhucCons) = c("Time","Survival","HState")
s2uhlcCons = data.frame(s2uht,s2uhlc,c("Uninfected"))
names(s2uhlcCons) = c("Time","Survival","HState")
s2ihmCons = data.frame(s2iht,s2ihm,c("Infected"))
names(s2ihmCons) = c("Time","Survival","HState")
s2ihucCons = data.frame(s2iht,s2ihuc,c("Infected"))
names(s2ihucCons) = c("Time","Survival","HState")
s2ihlcCons = data.frame(s2iht,s2ihlc,c("Infected"))
names(s2ihlcCons) = c("Time","Survival","HState")
s2mdat = rbind(s2uhmCons,s2ihmCons)
s2ucdat = rbind(s2uhucCons,s2ihucCons)
s2lcdat = rbind(s2uhlcCons,s2ihlcCons)
s2mdat$HState = as.factor(s2mdat$HState)
s2ucdat$HState = as.factor(s2ucdat$HState)
s2lcdat$HState = as.factor(s2lcdat$HState)

s02Plot<-ggplot()+
  geom_step(data=s2mdat,aes(x=Time,y=Survival,colour=HState))+
  geom_step(data=s2ucdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  geom_step(data=s2lcdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  annotate("text", x = 5, y = 0.95, label = "B")+
  ylim(0.7,1.0)+
  xlab("Time (Months)")+
  ylab("Survival")+
  labs(colour="Household Infection Status")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        legend.box.background = element_rect(color="black",size=1.2), axis.title.x = element_blank(),
        axis.text.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

# Next, s03.

s3nuh <- s03MP$strata[1]
s3toth <- length(s03MP$time)
s3uht <- s03MP$time[1:s3nuh]
s3uhm <- s03MP$surv[1:s3nuh]
s3uhuc <- s03MP$upper[1:s3nuh]
s3uhlc <- s03MP$lower[1:s3nuh]
s3iht <- s03MP$time[(s3nuh+1):s3toth]
s3ihm <- s03MP$surv[(s3nuh+1):s3toth]
s3ihuc <- s03MP$upper[(s3nuh+1):s3toth]
s3ihlc <- s03MP$lower[(s3nuh+1):s3toth]
# For the purposes of plotting, we need to add a single initial point for these.
s3uht = c(0,s3uht)
s3uhm = c(1,s3uhm)
s3uhuc = c(1,s3uhuc)
s3uhlc = c(1,s3uhlc)
s3iht = c(0,s3iht)
s3ihm = c(1,s3ihm)
s3ihuc = c(1,s3ihuc)
s3ihlc = c(1,s3ihlc)
# Turns out that figure legends only happen automatically. Time to combine these into 3 sets and then use those instead.
s3uhmCons = data.frame(s3uht,s3uhm,c("Uninfected"))
names(s3uhmCons) = c("Time","Survival","HState")
s3uhucCons = data.frame(s3uht,s3uhuc,c("Uninfected"))
names(s3uhucCons) = c("Time","Survival","HState")
s3uhlcCons = data.frame(s3uht,s3uhlc,c("Uninfected"))
names(s3uhlcCons) = c("Time","Survival","HState")
s3ihmCons = data.frame(s3iht,s3ihm,c("Infected"))
names(s3ihmCons) = c("Time","Survival","HState")
s3ihucCons = data.frame(s3iht,s3ihuc,c("Infected"))
names(s3ihucCons) = c("Time","Survival","HState")
s3ihlcCons = data.frame(s3iht,s3ihlc,c("Infected"))
names(s3ihlcCons) = c("Time","Survival","HState")
s3mdat = rbind(s3uhmCons,s3ihmCons)
s3ucdat = rbind(s3uhucCons,s3ihucCons)
s3lcdat = rbind(s3uhlcCons,s3ihlcCons)
s3mdat$HState = as.factor(s3mdat$HState)
s3ucdat$HState = as.factor(s3ucdat$HState)
s3lcdat$HState = as.factor(s3lcdat$HState)

s03Plot<-ggplot()+
  geom_step(data=s3mdat,aes(x=Time,y=Survival,colour=HState))+
  geom_step(data=s3ucdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  geom_step(data=s3lcdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  annotate("text", x = 5, y = 0.95, label = "C")+
  ylim(0.7,1.0)+
  xlab("Time (Months)")+
  ylab("Survival")+
  labs(colour="Household Infection Status")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.25,0.19),
        legend.box.background = element_rect(color="black",size=1.2))

# Finally, s04.

s4nuh <- s04MP$strata[1]
s4toth <- length(s04MP$time)
s4uht <- s04MP$time[1:s4nuh]
s4uhm <- s04MP$surv[1:s4nuh]
s4uhuc <- s04MP$upper[1:s4nuh]
s4uhlc <- s04MP$lower[1:s4nuh]
s4iht <- s04MP$time[(s4nuh+1):s4toth]
s4ihm <- s04MP$surv[(s4nuh+1):s4toth]
s4ihuc <- s04MP$upper[(s4nuh+1):s4toth]
s4ihlc <- s04MP$lower[(s4nuh+1):s4toth]
# For the purposes of plotting, we need to add a single initial point for these.
s4uht = c(0,s4uht)
s4uhm = c(1,s4uhm)
s4uhuc = c(1,s4uhuc)
s4uhlc = c(1,s4uhlc)
s4iht = c(0,s4iht)
s4ihm = c(1,s4ihm)
s4ihuc = c(1,s4ihuc)
s4ihlc = c(1,s4ihlc)
# Turns out that figure legends only happen automatically. Time to combine these into 3 sets and then use those instead.
s4uhmCons = data.frame(s4uht,s4uhm,c("Uninfected"))
names(s4uhmCons) = c("Time","Survival","HState")
s4uhucCons = data.frame(s4uht,s4uhuc,c("Uninfected"))
names(s4uhucCons) = c("Time","Survival","HState")
s4uhlcCons = data.frame(s4uht,s4uhlc,c("Uninfected"))
names(s4uhlcCons) = c("Time","Survival","HState")
s4ihmCons = data.frame(s4iht,s4ihm,c("Infected"))
names(s4ihmCons) = c("Time","Survival","HState")
s4ihucCons = data.frame(s4iht,s4ihuc,c("Infected"))
names(s4ihucCons) = c("Time","Survival","HState")
s4ihlcCons = data.frame(s4iht,s4ihlc,c("Infected"))
names(s4ihlcCons) = c("Time","Survival","HState")
s4mdat = rbind(s4uhmCons,s4ihmCons)
s4ucdat = rbind(s4uhucCons,s4ihucCons)
s4lcdat = rbind(s4uhlcCons,s4ihlcCons)
s4mdat$HState = as.factor(s4mdat$HState)
s4ucdat$HState = as.factor(s4ucdat$HState)
s4lcdat$HState = as.factor(s4lcdat$HState)

s04Plot<-ggplot()+
  geom_step(data=s4mdat,aes(x=Time,y=Survival,colour=HState))+
  geom_step(data=s4ucdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  geom_step(data=s4lcdat,aes(x=Time,y=Survival,colour=HState),linetype=2)+
  annotate("text", x = 5, y = 0.95, label = "D")+
  ylim(0.7,1.0)+
  xlab("Time (Months)")+
  ylab("Survival")+
  labs(colour="Household Infection Status")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
        legend.box.background = element_rect(color="black",size=1.2), axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Each of those is missing part of the graph (except s03, which is intact). This is so we can use grid.arrange.

fullGridPlot = grid.arrange(grobs=list(s01Plot,s02Plot,s03Plot,s04Plot), nrow = 2, ncol = 2)
