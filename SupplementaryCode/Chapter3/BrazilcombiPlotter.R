# This is the combination plotter script that makes grid.arrange based plots with both the splines and the histograms of
# the SVI values.

library(ggplot2)
library(gridExtra)
library(MASS)
library(plyr)
library(survival)

setwd('~/LeishmaniaPhDStuff/Chapman2018CPWork')

# The easiest way to approach this that sprung to mind was to set some threshold at which a municipality is considered
# to be 'endemic' for leishmaniasis. It was be beneficial to start with official thresholds if they can be found, and use
# some of our own choices.

# According to the Brazilian Govt. docs, a sensible threshold might be 3.0 cases per 100000 people. Well, that's inspired
# by them, anyway, since they use multiple categories but we're only allowed to fail once and in one way. It will be worth
# using some higher and lower thresholds too.

# Maybe worth trying to use 5.0 cases per 100000 people, since it is quite substantially higher (it's even bigger than
# the uppermost category of transmission). Also could try 1 per 100000 people but that will likely be too small.

brazilCaseDat <- read.csv('RibeiroBrazilDataClipped.csv',header=TRUE) # Inelegant replacement, but the easier option.
brazilSVIDat <- read.csv('RibeiroBrazilDataSVIClipped.csv',header=TRUE)
# NOTE: Removed entries 231, 354, and 899 due to errors in data formatting, which led to unusable data.
# Mostly, it was some strange formatting of () that led to the data becoming characters instead of numbers.

# This still needs a lot of processing to get it into a usable format.
# Also note, 1794 municipalities is much nicer than the ~47 million people they contain...

# so, for each district we need the number of cases per 100000 people for each year. We then take year take t = 4 as our
# start time (since we need to be based on the three-year average).

infRateDat <- c()
discSVIDat <- c()
for(i in 1:1791){
  infRateVec = c()
  discSVIVec = c()
  indVec = brazilCaseDat[i,]
  y1r = indVec$CAS_00 * (100000/(indVec$POP_00))
  y2r = indVec$CAS_01 * (100000/(indVec$POP_01))
  y3r = indVec$CAS_02 * (100000/(indVec$POP_02))
  y4r = indVec$CAS_03 * (100000/(indVec$POP_03))
  y5r = indVec$CAS_04 * (100000/(indVec$POP_04))
  y6r = indVec$CAS_05 * (100000/(indVec$POP_05))
  y7r = indVec$CAS_06 * (100000/(indVec$POP_06))
  y8r = indVec$CAS_07 * (100000/(indVec$POP_07))
  y9r = indVec$CAS_08 * (100000/(indVec$POP_08))
  y10r = indVec$CAS_09 * (100000/(indVec$POP_09))
  y11r = indVec$CAS_10 * (100000/(indVec$POP_10))
  y12r = indVec$CAS_11 * (100000/(indVec$POP_11))
  y13r = indVec$CAS_12 * (100000/(indVec$POP_12))
  y14r = indVec$CAS_13 * (100000/(indVec$POP_13))
  y15r = indVec$CAS_14 * (100000/(indVec$POP_14))
  y16r = indVec$CAS_15 * (100000/(indVec$POP_15))
  y17r = indVec$CAS_16 * (100000/(indVec$POP_16))
  y18r = indVec$CAS_17 * (100000/(indVec$POP_17))
  # A set of incidence rates for each year.
  # With these, I want to create a set of three-year averages.
  av1 = (y1r+y2r+y3r)/3
  av2 = (y2r+y3r+y4r)/3
  av3 = (y3r+y4r+y5r)/3
  av4 = (y4r+y5r+y6r)/3
  av5 = (y5r+y6r+y7r)/3
  av6 = (y6r+y7r+y8r)/3
  av7 = (y7r+y8r+y9r)/3
  av8 = (y8r+y9r+y10r)/3
  av9 = (y9r+y10r+y11r)/3
  av10 = (y10r+y11r+y12r)/3
  av11 = (y11r+y12r+y13r)/3
  av12 = (y12r+y13r+y14r)/3
  av13 = (y13r+y14r+y15r)/3
  av14 = (y14r+y15r+y16r)/3
  av15 = (y15r+y16r+y17r)/3
  av16 = (y16r+y17r+y18r)/3
  # These look like, for example, av1 corresponds to the first year. But we offset the fist year to year 3, and are
  # thus looking back in time. av16 is the 3-year average as calculated at the end of the final year.
  # The 'survival time' is the time until the 1st 3-year average is greater than our chosen threshold. Since there
  # will be multiple thresholds possible, I'll not calculate it yet.
  # I also want to process the SVI into categories. We can use it as a raw variable but the government categorises it.
  sviVec = brazilSVIDat[i,]
  if(sviVec$SVI <= 0.2){
    sviO = 0
  }
  if(sviVec$SVI > 0.2 && sviVec$SVI <= 0.3){
    sviO = 1
  }
  if(sviVec$SVI > 0.3 && sviVec$SVI <= 0.4){
    sviO = 2
  }
  if(sviVec$SVI > 0.4 && sviVec$SVI <= 0.5){
    sviO = 3
  }
  if(sviVec$SVI > 0.5){
    sviO = 4
  }
  # The same again for the 3 'dimensions' of the SVI.
  # Urban Infrastructure.
  if(sviVec$SVI.IU <= 0.2){
    sviIU = 0
  }
  if(sviVec$SVI.IU > 0.2 && sviVec$SVI.IU <= 0.3){
    sviIU = 1
  }
  if(sviVec$SVI.IU > 0.3 && sviVec$SVI.IU <= 0.4){
    sviIU = 2
  }
  if(sviVec$SVI.IU > 0.4 && sviVec$SVI.IU <= 0.5){
    sviIU = 3
  }
  if(sviVec$SVI.IU > 0.5){
    sviIU = 4
  }
  # Human Capital
  if(sviVec$SVI.HC <= 0.2){
    sviHC = 0
  }
  if(sviVec$SVI.HC > 0.2 && sviVec$SVI.HC <= 0.3){
    sviHC = 1
  }
  if(sviVec$SVI.HC > 0.3 && sviVec$SVI.HC <= 0.4){
    sviHC = 2
  }
  if(sviVec$SVI.HC > 0.4 && sviVec$SVI.HC <= 0.5){
    sviHC = 3
  }
  if(sviVec$SVI.HC > 0.5){
    sviHC = 4
  }
  # Income/Work
  if(sviVec$SVI.I.W <= 0.2){
    sviIW = 0
  }
  if(sviVec$SVI.I.W > 0.2 && sviVec$SVI.I.W <= 0.3){
    sviIW = 1
  }
  if(sviVec$SVI.I.W > 0.3 && sviVec$SVI.I.W <= 0.4){
    sviIW = 2
  }
  if(sviVec$SVI.I.W > 0.4 && sviVec$SVI.I.W <= 0.5){
    sviIW = 3
  }
  if(sviVec$SVI.I.W > 0.5){
    sviIW = 4
  }
  # This gives us a set of 4 covariates, all discrete. It will be possible to fit splines to the original versions too.
  # Now we assemble these into entries for the two new dataframes. We aren't quite finished processing one, but will
  # complete the processing in another loop to make it easier to decide on thresholds etc. to calculate the survival times.
  infRateVec = c(i, av1, av2, av3, av4, av5, av6, av7, av8, av9, av10, av11, av12, av13, av14, av15, av16)
  discSVIVec = c(i, sviO, sviIU, sviHC, sviIW)
  infRateDat = rbind(infRateDat, infRateVec)
  discSVIDat = rbind(discSVIDat, discSVIVec)
}
infRateDat = data.frame(infRateDat)
discSVIDat = data.frame(discSVIDat)
names(infRateDat) = c("Indv","av1","av2","av3","av4","av5","av6","av7","av8","av9","av10","av11","av12","av13","av14","av15","av16")
names(discSVIDat) = c("Indv","SVIO","SVIIU","SVIHC","SVIIW")
infRateDatRef = infRateDat
discSVIDatRef = discSVIDat

threshold = 2.0 # Creatively named, I know. A few Brazilian-Govt choices are: 2.2, 4.4. We're going with 3.0 for now.
# Some districts will already have 'failed' at the start. av1 represents the initial 3-year average at t=0. We subset
# to remove all municipalities with av1 > threshold.
infRateDat = subset(infRateDat, infRateDat$av1 <= threshold) # Ha, originally this was broken...
discSVIDat = discSVIDat[infRateDat$Indv,]
contSVIDat = brazilSVIDat[infRateDat$Indv,]
# Next we loop through the averages to find the first one that exceeds the threshold, then record the time as the
# survival time. This is going to be a bit messy and will have to use a while loop.
surDat = c()
for(i in 1:length(infRateDat$Indv)){
  aveVec = infRateDat[i,3:17]
  preAve = infRateDat$av1[i]
  timecounter = 1
  aveInd = 1
  infState = 0
  censor = 0
  while(preAve < threshold && aveInd < 16){
    newAve = aveVec[aveInd]
    if(is.na(newAve) == TRUE){
      infState = 0
      censor = 1 # An indicator that says this municipality was censored.
      break
    }
    if(newAve < threshold){
      timecounter = timecounter + 1
      aveInd = aveInd + 1
    }
    preAve = newAve
  }
  if(timecounter == 16 || censor == 1){
    infState = 0
  }
  else{
    infState = 1
  }
  surVec = c(infRateDat$Indv[i],timecounter,infState)
  surDat = rbind(surDat, surVec)
}
surDatCont = cbind(surDat, contSVIDat[,3:6])
surDat = cbind(surDat, discSVIDat[,2:5])

# Hopefully the loop didn't get stuck, and we now have a nice set of survival times.
surDat = data.frame(surDat)
names(surDat) = c("Indv","SurTime","InfState","SVIO","SVIIU","SVIHC","SVIIW")
surDatCont = data.frame(surDatCont)
names(surDatCont) = c("Indv","SurTime","InfState","SVIO","SVIIU","SVIHC","SVIIW")

cphsvio2t0dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIO,df=0),data=surDatCont)
cphsvio2t0dcont

svio2t0dtemp <- termplot(cphsvio2t0dcont, term=1, se=TRUE, plot = FALSE)
xterm = svio2t0dtemp$SVIO
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svio2t0dmat = cbind(xterm$x,ymat)
names(svio2t0dmat) = c("SVI","AveHaz","LowCI","UpCI")

svio2t0dspline <- ggplot(data = svio2t0dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.80)+
  xlab("Overall SVI")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svio2histcb <- ggplot()+
  geom_histogram(aes(x = SVI), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.8)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svio2t0dgrid <- grid.arrange(grobs = list(svio2t0dspline,svio2histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

cphsvio2t3dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIO,df=3),data=surDatCont)
cphsvio2t3dcont

svio2t3dtemp <- termplot(cphsvio2t3dcont, term=1, se=TRUE, plot = FALSE)
xterm = svio2t3dtemp$SVIO
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svio2t3dmat = cbind(xterm$x,ymat)
names(svio2t3dmat) = c("SVI","AveHaz","LowCI","UpCI")

svio2t3dspline <- ggplot(data = svio2t3dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.80)+
  xlab("Overall SVI")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svio2histcb <- ggplot()+
  geom_histogram(aes(x = SVI), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.8)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svio2t3dgrid <- grid.arrange(grobs = list(svio2t3dspline,svio2histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

infRateDat = infRateDatRef
discSVIDat = discSVIDatRef
# Yes, that's right, I'm doing 4.5 after 2.0. Purely for saving a little bit of space in my code.
threshold = 4.5 # Creatively named, I know. A few Brazilian-Govt choices are: 2.2, 4.4. We're going with 3.0 for now.
# Some districts will already have 'failed' at the start. av1 represents the initial 3-year average at t=0. We subset
# to remove all municipalities with av1 > threshold.
infRateDat = subset(infRateDat, infRateDat$av1 <= threshold) # Ha, originally this was broken...
discSVIDat = discSVIDat[infRateDat$Indv,]
contSVIDat = brazilSVIDat[infRateDat$Indv,]
# Next we loop through the averages to find the first one that exceeds the threshold, then record the time as the
# survival time. This is going to be a bit messy and will have to use a while loop.
surDat = c()
for(i in 1:length(infRateDat$Indv)){
  aveVec = infRateDat[i,3:17]
  preAve = infRateDat$av1[i]
  timecounter = 1
  aveInd = 1
  infState = 0
  censor = 0
  while(preAve < threshold && aveInd < 16){
    newAve = aveVec[aveInd]
    if(is.na(newAve) == TRUE){
      infState = 0
      censor = 1 # An indicator that says this municipality was censored.
      break
    }
    if(newAve < threshold){
      timecounter = timecounter + 1
      aveInd = aveInd + 1
    }
    preAve = newAve
  }
  if(timecounter == 16 || censor == 1){
    infState = 0
  }
  else{
    infState = 1
  }
  surVec = c(infRateDat$Indv[i],timecounter,infState)
  surDat = rbind(surDat, surVec)
}
surDatCont = cbind(surDat, contSVIDat[,3:6])
surDat = cbind(surDat, discSVIDat[,2:5])

# Hopefully the loop didn't get stuck, and we now have a nice set of survival times.
surDat = data.frame(surDat)
names(surDat) = c("Indv","SurTime","InfState","SVIO","SVIIU","SVIHC","SVIIW")
surDatCont = data.frame(surDatCont)
names(surDatCont) = c("Indv","SurTime","InfState","SVIO","SVIIU","SVIHC","SVIIW")

cphsvio45t0dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIO,df=0),data=surDatCont)
cphsvio45t0dcont

svio45t0dtemp <- termplot(cphsvio45t0dcont, term=1, se=TRUE, plot = FALSE)
xterm = svio45t0dtemp$SVIO
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svio45t0dmat = cbind(xterm$x,ymat)
names(svio45t0dmat) = c("SVI","AveHaz","LowCI","UpCI")

svio45t0dspline <- ggplot(data = svio45t0dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.80)+
  xlab("Overall SVI")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svio45histcb <- ggplot()+
  geom_histogram(aes(x = SVI), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.8)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svio45t0dgrid <- grid.arrange(grobs = list(svio45t0dspline,svio45histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

cphsvio45t3dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIO,df=3),data=surDatCont)
cphsvio45t3dcont

svio45t3dtemp <- termplot(cphsvio45t3dcont, term=1, se=TRUE, plot = FALSE)
xterm = svio45t3dtemp$SVIO
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svio45t3dmat = cbind(xterm$x,ymat)
names(svio45t3dmat) = c("SVI","AveHaz","LowCI","UpCI")

svio45t3dspline <- ggplot(data = svio45t3dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.80)+
  xlab("Overall SVI")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svio45histcb <- ggplot()+
  geom_histogram(aes(x = SVI), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.8)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svio45t3dgrid <- grid.arrange(grobs = list(svio45t3dspline,svio45histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

infRateDat = infRateDatRef
discSVIDat = discSVIDatRef
# Now we do 3.0, because we plot additional graphs for this one without needing to rerun some loops.
threshold = 3.0 # Creatively named, I know. A few Brazilian-Govt choices are: 2.2, 4.4. We're going with 3.0 for now.
# Some districts will already have 'failed' at the start. av1 represents the initial 3-year average at t=0. We subset
# to remove all municipalities with av1 > threshold.
infRateDat = subset(infRateDat, infRateDat$av1 <= threshold) # Ha, originally this was broken...
discSVIDat = discSVIDat[infRateDat$Indv,]
contSVIDat = brazilSVIDat[infRateDat$Indv,]
# Next we loop through the averages to find the first one that exceeds the threshold, then record the time as the
# survival time. This is going to be a bit messy and will have to use a while loop.
surDat = c()
for(i in 1:length(infRateDat$Indv)){
  aveVec = infRateDat[i,3:17]
  preAve = infRateDat$av1[i]
  timecounter = 1
  aveInd = 1
  infState = 0
  censor = 0
  while(preAve < threshold && aveInd < 16){
    newAve = aveVec[aveInd]
    if(is.na(newAve) == TRUE){
      infState = 0
      censor = 1 # An indicator that says this municipality was censored.
      break
    }
    if(newAve < threshold){
      timecounter = timecounter + 1
      aveInd = aveInd + 1
    }
    preAve = newAve
  }
  if(timecounter == 16 || censor == 1){
    infState = 0
  }
  else{
    infState = 1
  }
  surVec = c(infRateDat$Indv[i],timecounter,infState)
  surDat = rbind(surDat, surVec)
}
surDatCont = cbind(surDat, contSVIDat[,3:6])
surDat = cbind(surDat, discSVIDat[,2:5])

# Hopefully the loop didn't get stuck, and we now have a nice set of survival times.
surDat = data.frame(surDat)
names(surDat) = c("Indv","SurTime","InfState","SVIO","SVIIU","SVIHC","SVIIW")
surDatCont = data.frame(surDatCont)
names(surDatCont) = c("Indv","SurTime","InfState","SVIO","SVIIU","SVIHC","SVIIW")

cphsvio3t0dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIO,df=0),data=surDatCont)
cphsvio3t0dcont

svio3t0dtemp <- termplot(cphsvio3t0dcont, term=1, se=TRUE, plot = FALSE)
xterm = svio3t0dtemp$SVIO
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svio3t0dmat = cbind(xterm$x,ymat)
names(svio3t0dmat) = c("SVI","AveHaz","LowCI","UpCI")

svio3t0dspline <- ggplot(data = svio3t0dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.80)+
  xlab("Overall SVI")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svio3histcb <- ggplot()+
  geom_histogram(aes(x = SVI), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.8)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svio3t0dgrid <- grid.arrange(grobs = list(svio3t0dspline,svio3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

cphsvio3t3dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIO,df=3),data=surDatCont)
cphsvio3t3dcont

svio3t3dtemp <- termplot(cphsvio3t3dcont, term=1, se=TRUE, plot = FALSE)
xterm = svio3t3dtemp$SVIO
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svio3t3dmat = cbind(xterm$x,ymat)
names(svio3t3dmat) = c("SVI","AveHaz","LowCI","UpCI")

svio3t3dspline <- ggplot(data = svio3t3dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.80)+
  xlab("Overall SVI")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svio3histcb <- ggplot()+
  geom_histogram(aes(x = SVI), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.8)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svio3t3dgrid <- grid.arrange(grobs = list(svio3t3dspline,svio3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

# Next up is a couple of grids for the urban infrastructure component.
# NOTE: the data refers to it as "SVI.IU" since it wasn't in English, we keep that notation internally for consistency.
cphsviiu3t0dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIIU,df=0),data=surDatCont)
cphsviiu3t0dcont

sviiu3t0dtemp <- termplot(cphsviiu3t0dcont, term=1, se=TRUE, plot = FALSE)
xterm = sviiu3t0dtemp$SVIIU
center <- with(xterm, y[x==0.502])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
sviiu3t0dmat = cbind(xterm$x,ymat)
names(sviiu3t0dmat) = c("SVI","AveHaz","LowCI","UpCI")

sviiu3t0dspline <- ggplot(data = sviiu3t0dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.0,1.0)+
  xlab("SVI (Urban Infrastructure)")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

sviiu3histcb <- ggplot()+
  geom_histogram(aes(x = SVI.IU), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("SVI (Urban Infrastructure)")+
  ylab("Frequency")+
  xlim(0.0,1.0)+
  ylim(300,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

sviiu3t0dgrid <- grid.arrange(grobs = list(sviiu3t0dspline,sviiu3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

cphsviiu3t3dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIIU,df=3),data=surDatCont)
cphsviiu3t3dcont

sviiu3t3dtemp <- termplot(cphsviiu3t3dcont, term=1, se=TRUE, plot = FALSE)
xterm = sviiu3t3dtemp$SVIIU
center <- with(xterm, y[x==0.502])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
sviiu3t3dmat = cbind(xterm$x,ymat)
names(sviiu3t3dmat) = c("SVI","AveHaz","LowCI","UpCI")

sviiu3t3dspline <- ggplot(data = sviiu3t3dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.0,1.0)+
  xlab("SVI (Urban Infrastructure)")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Not updating the xlab in the histograms because we actually remove it anyway.
sviiu3histcb <- ggplot()+
  geom_histogram(aes(x = SVI.IU), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.0,1.0)+
  ylim(300,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

sviiu3t3dgrid <- grid.arrange(grobs = list(sviiu3t3dspline,sviiu3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

# Next, the human capital component.
cphsvihc3t0dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIHC,df=0),data=surDatCont)
cphsvihc3t0dcont

svihc3t0dtemp <- termplot(cphsvihc3t0dcont, term=1, se=TRUE, plot = FALSE)
xterm = svihc3t0dtemp$SVIHC
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svihc3t0dmat = cbind(xterm$x,ymat)
names(svihc3t0dmat) = c("SVI","AveHaz","LowCI","UpCI")

svihc3t0dspline <- ggplot(data = svihc3t0dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.15,0.85)+
  xlab("SVI (Human Capital)")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

svihc3histcb <- ggplot()+
  geom_histogram(aes(x = SVI.HC), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("SVI (Human Capital)")+
  ylab("Frequency")+
  xlim(0.1,0.9)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svihc3t0dgrid <- grid.arrange(grobs = list(svihc3t0dspline,svihc3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

cphsvihc3t3dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIHC,df=3),data=surDatCont)
cphsvihc3t3dcont

svihc3t3dtemp <- termplot(cphsvihc3t3dcont, term=1, se=TRUE, plot = FALSE)
xterm = svihc3t3dtemp$SVIHC
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
svihc3t3dmat = cbind(xterm$x,ymat)
names(svihc3t3dmat) = c("SVI","AveHaz","LowCI","UpCI")

svihc3t3dspline <- ggplot(data = svihc3t3dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.15,0.85)+
  xlab("SVI (Human Capital)")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Not updating the xlab in the histograms because we actually remove it anyway.
svihc3histcb <- ggplot()+
  geom_histogram(aes(x = SVI.HC), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.9)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

svihc3t3dgrid <- grid.arrange(grobs = list(svihc3t3dspline,svihc3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

# Finally, the Income/Work component.

cphsviiw3t0dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIIW,df=0),data=surDatCont)
cphsviiw3t0dcont

sviiw3t0dtemp <- termplot(cphsviiw3t0dcont, term=1, se=TRUE, plot = FALSE)
xterm = sviiw3t0dtemp$SVIIW
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
sviiw3t0dmat = cbind(xterm$x,ymat)
names(sviiw3t0dmat) = c("SVI","AveHaz","LowCI","UpCI")

sviiw3t0dspline <- ggplot(data = sviiw3t0dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.85)+
  xlab("SVI (Income/Work)")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

sviiw3histcb <- ggplot()+
  geom_histogram(aes(x = SVI.I.W), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("SVI (Income/Work)")+
  ylab("Frequency")+
  xlim(0.1,0.9)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

sviiw3t0dgrid <- grid.arrange(grobs = list(sviiw3t0dspline,sviiw3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

cphsviiw3t3dcont <- coxph(Surv(SurTime,InfState)~pspline(SVIIW,df=3),data=surDatCont)
cphsviiw3t3dcont

sviiw3t3dtemp <- termplot(cphsviiw3t3dcont, term=1, se=TRUE, plot = FALSE)
xterm = sviiw3t3dtemp$SVIIW
center <- with(xterm, y[x==0.5])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
ymat = data.frame(exp(ytemp - center))
sviiw3t3dmat = cbind(xterm$x,ymat)
names(sviiw3t3dmat) = c("SVI","AveHaz","LowCI","UpCI")

sviiw3t3dspline <- ggplot(data = sviiw3t3dmat)+
  geom_line(aes(x = SVI, y = AveHaz))+
  geom_line(aes(x = SVI, y = LowCI), linetype = 2)+
  geom_line(aes(x = SVI, y = UpCI), linetype = 2)+
  xlim(0.1,0.85)+
  xlab("SVI (Income/Work)")+
  ylab("Relative Hazard")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Not updating the xlab in the histograms because we actually remove it anyway.
sviiw3histcb <- ggplot()+
  geom_histogram(aes(x = SVI.I.W), data = contSVIDat, binwidth = 0.05,fill = "#69b3a2",color="#000000")+
  xlab("Overall SVI")+
  ylab("Frequency")+
  xlim(0.1,0.9)+
  ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

sviiw3t3dgrid <- grid.arrange(grobs = list(sviiw3t3dspline,sviiw3histcb), nrow = 2, ncol = 1, heights = c(1.0,0.5))

# Created as of 23/05/2022.
# This is going to be an extension of the combi plotter - a single, tripartite figure showing the correlation of the
# different SVI components. It may or may not turn out to be important.

# First a really rough run of cor.test, I'm using Pearson's product-moment correlation for now, but will consult
# with those who know more and update accordingly.

# SVI-UI X SVI-HC
cor.test(brazilSVIDat$SVI.IU,brazilSVIDat$SVI.HC)
# SVI-UI X SVI-I/W
cor.test(brazilSVIDat$SVI.IU,brazilSVIDat$SVI.I.W)
# SVI-I/W X SVI-HC
cor.test(brazilSVIDat$SVI.I.W,brazilSVIDat$SVI.HC)

# Next we assemble all the components of the plot. Eugh, all three need both axes labelled, which might look worse.
# Then we arrange them all in a row and hope the resulting plot isn't awful.
# A second idea to try too: one plot but with different colours/shapes.
# A third idea (woo, bonanza of ideas!): a 2x2 grid with one quarter being a key.

uihccorplot <- ggplot()+
  geom_point(aes(x = SVI.IU, y = SVI.HC), data = brazilSVIDat, color="#000000")+
  xlab("SVI - Urban Infrastructure")+
  ylab("SVI - Human Capital")+
  xlim(0.0,1.0)+
  ylim(0.0,1.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

uiiwcorplot <- ggplot()+
  geom_point(aes(x = SVI.IU, y = SVI.I.W), data = brazilSVIDat, color="#000000")+
  xlab("SVI - Urban Infrastructure")+
  ylab("SVI - Income / Work")+
  xlim(0.0,1.0)+
  ylim(0.0,1.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

iwhccorplot <- ggplot()+
  geom_point(aes(x = SVI.I.W, y = SVI.HC), data = brazilSVIDat, color="#000000")+
  xlab("SVI - Income / Work")+
  ylab("SVI - Human Capital")+
  xlim(0.0,1.0)+
  ylim(0.0,1.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

corplotgrid <- grid.arrange(grobs = list(uihccorplot,iwhccorplot,uiiwcorplot), nrow = 2, ncol = 2)
