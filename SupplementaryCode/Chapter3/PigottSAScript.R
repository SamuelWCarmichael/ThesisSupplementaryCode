# This script performs some survival analysis on a couple of other datasets of interest, mostly on a database from Pigott.
# It is largely intended to show that while you can perform survival analysis on it, despite its simplicity, you do not
# necessarily always yield meaning results and may come up against the limits of the method.
library(ggplot2)
library(gridExtra)
library(MASS)
library(plyr)
library(survival)

setwd('~/LeishmaniaPhDStuff/Chapman2018CPWork')

PigottCLDat <- read.csv('PigottCLDatCSV.csv', header = TRUE)

# With minimal processing we can make this suitable for use.

# First we remove all the 'polygon' entries - these correspond to something other than an individual, and aren't useful.
PigottCLDatTrim <- subset(PigottCLDat, PigottCLDat$LOCATION_TYPE == "point")
# The year variable will make a fine enough survival time, if we adjust it a little.
PigottCLDatTrim$YEAR <- PigottCLDatTrim$YEAR - (min(PigottCLDatTrim$YEAR)-1) # Here we imagine we started monitoring the year before the minimum.
# This is a good example of why the set isn't suitable - we haven't actually been monitoring the areas here, so this isn't quite accurate.
# A final addition: a basically dummy infection status row where all entries are 1.
infVec <- c()
for(i in 1:4248){
  infVec = rbind(infVec, 1)
}
PigottCLDatTrim = cbind(PigottCLDatTrim, infVec)
# Now we demonstrate that although it works, survival analysis isn't especially helpful here.
pigottxspline <- coxph(Surv(YEAR,infVec)~pspline(X,df=0),data=PigottCLDatTrim)
pigottxspline

pigottxtemp <- termplot(pigottxspline, term=1, se=TRUE, plot = FALSE)
xterm = pigottxtemp$X
center <- with(xterm, y[1500])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
matplot(xterm$x, exp(ytemp - center),
        type='l', lty=c(1,2,2), col=1,
        xlab="X", ylab="Relative hazard")

# Success! A lovely graph BUT peaks in hazard typically coincide with regions that have a big presence in the literature.
# This is a consequence of how the database was constructed, and shows me that although we can us SA here, it doesn't
# tell us anything interesting because the data wasn't really intended for this purpose.

# Bonus: this shows a limit of the spline fitting method, which might be useful elsewhere.
# Worth examining the y-covariate too. I think we'll see something similar though.

pigottyspline <- coxph(Surv(YEAR,infVec)~pspline(Y,df=0),data=PigottCLDatTrim)
pigottyspline

pigottytemp <- termplot(pigottyspline, term=1, se=TRUE, plot = FALSE)
xterm = pigottytemp$Y
center <- with(xterm, y[1500])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
matplot(xterm$x, exp(ytemp - center),
        type='l', lty=c(1,2,2), col=1,
        xlab="Latitude", ylab="Relative hazard")

# I'm curious to know what coxph will do if I try to give it both x and y as covariate...
# Uh, it doesn't approve - not sure if it can actually fit such a thing.

# To summarise: for CL, we see various peaks in hazard in terms of x and y. These seem to correspond to regions with
# a lot of published studies, and this might not be a reflection of the actually relative hazards. Thus, this analysis
# is possible but not useful to us.

# Now for the VL data:

PigottVLDat <- read.csv('PigottVLDatCSV.csv', header = TRUE)

# With minimal processing we can make this suitable for use.

# First we remove all the 'polygon' entries - these correspond to something other than an individual, and aren't useful.
PigottVLDatTrim <- subset(PigottVLDat, PigottVLDat$LOCATION_TYPE == "point")
# The year variable will make a fine enough survival time, if we adjust it a little.
PigottVLDatTrim$YEAR <- PigottVLDatTrim$YEAR - (min(PigottVLDatTrim$YEAR)-1) # Here we imagine we started monitoring the year before the minimum.
# This is a good example of why the set isn't suitable - we haven't actually been monitoring the areas here, so this isn't quite accurate.
# A final addition: a basically dummy infection status row where all entries are 1.
infVec <- c()
for(i in 1:3514){
  infVec = rbind(infVec, 1)
}
PigottVLDatTrim = cbind(PigottVLDatTrim, infVec)
# Now we demonstrate that although it works, survival analysis isn't especially helpful here.
pigottxspline <- coxph(Surv(YEAR,infVec)~pspline(X,df=0),data=PigottVLDatTrim)
pigottxspline

pigottxtemp <- termplot(pigottxspline, term=1, se=TRUE, plot = FALSE)
xterm = pigottxtemp$X
center <- with(xterm, y[500])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
matplot(xterm$x, exp(ytemp - center),
        type='l', lty=c(1,2,2), col=1,
        xlab="X", ylab="Relative hazard")

# Success! A lovely graph BUT peaks in hazard typically coincide with regions that have a big presence in the literature.
# This is a consequence of how the database was constructed, and shows me that although we can us SA here, it doesn't
# tell us anything interesting because the data wasn't really intended for this purpose.

# Bonus: this shows a limit of the spline fitting method, which might be useful elsewhere.
# Worth examining the y-covariate too. I think we'll see something similar though.

pigottyspline <- coxph(Surv(YEAR,infVec)~pspline(Y,df=0),data=PigottVLDatTrim)
pigottyspline

pigottytemp <- termplot(pigottyspline, term=1, se=TRUE, plot = FALSE)
xterm = pigottytemp$Y
center <- with(xterm, y[500])
ytemp <- xterm$y + outer(xterm$se, c(0, -1.96, 1.96), '*')
matplot(xterm$x, exp(ytemp - center),
        type='l', lty=c(1,2,2), col=1,
        xlab="Latitude", ylab="Relative hazard")

# I'm curious to know what coxph will do if I try to give it both x and y as covariate...
# Uh, it doesn't approve - not sure if it can actually fit such a thing.

# To summarise: for CL, we see various peaks in hazard in terms of x and y. These seem to correspond to regions with
# a lot of published studies, and this might not be a reflection of the actually relative hazards. Thus, this analysis
# is possible but not useful to us.

# Going to plot a histogram of the distribution of cases across the x coordinate. It will highlight that the high s.e.
# regions coincide with areas of little data.

pigottxhist <- ggplot()+
  geom_histogram(aes(x = X), data = PigottCLDatTrim, binwidth = 5,fill = "#69b3a2",color="#000000")+
  xlab("X-Coordinate")+
  ylab("Frequency")+
  #xlim(0.1,0.8)+
  #ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pigottyhist <- ggplot()+
  geom_histogram(aes(x = Y), data = PigottCLDatTrim, binwidth = 5,fill = "#69b3a2",color="#000000")+
  xlab("Latitude")+
  ylab("Frequency")+
  #xlim(0.1,0.8)+
  #ylim(400,0.0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
