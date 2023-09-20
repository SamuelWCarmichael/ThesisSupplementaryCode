# Additional graphs considering things such as sensitivity analysis etc.
# Some of this code is for figures S7 and S8.

# Sensitivity analysis is based on Holt et al 2006, using: Ea = (a/lambda)(dlambda/da)

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)

setwd('~/LeishmaniaPhDStuff/SubmissionCollection/Codebase/NewCode/SimOutputRAnalysis')

# So, here's the plan. For each mouse, we load in the data, and calculate a mean R0 value for each indicator value.
# We also need to do this for a 'vanilla' set where we change no parameters. We have the parameter values and always
# change them by +20%. Once we hae the mean R0 values, we use the formula for Ea above to estimate elasticity. We plot
# each mouse as a separate bar chart, and we can combine all these charts into one big 4x5 grid (or maybe a few small ones.)
# Then we can nicely observe how the mean R0 changes with different parameters on a mouse by mouse basis.

# Data is formatted as: indicator, lifespan, numBites, <parasite array>. No headings but we can use v1 etc, the defaults.

# A simple function that evaluates if a bite causes a transmission. Used shortly to calculate a mean R0.
R0Calc <- function(N){
  NT = 0
  p = 0
  # Method 1: Smooth
  if(N > 1){
    p = 0.5*(tanh(0.015*(N-200))+1)
  }
  # Method 2: Binary
  #if(N >= 200){
  #  p = 1.1 # Slightly hacky method of saying "we transmit".
  #}
  #else{
  #  p = 0.0 # Slightly hacky method of saying "we don't transmit".
  #}
  q = runif(1)
  if(q < p){
    NT = 1
  }
}

# So, next up is a heatmap of the GRL model with 100% chance of biting an infected host and my newly implemented
# lifespan reduction mechanic. Once infected, the remaining lifespan is reduced by some proportion.

KVec = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
PBVec = c(1000,5950,10900,15850,20800,25750,30700,35650,40600,45550,50500,55450,60400,65350,70300,75250,80200,85150,90100,95050,100000)

dat = read.csv('GRL100PIM.csv',header = FALSE, sep = ',')
GRL100PIMHMList = c()
for(k in KVec){
  for(pb in PBVec){
    KPBDat = subset(dat, dat$V1 == k)
    KPBDat = subset(KPBDat, KPBDat$V2 == pb)
    R0List = c() # Empty vector to add our Num. Trans. to prior to calculating our mean R0.
    for(i in 1:length(KPBDat$V1)){
      nt = 0
      FV = KPBDat[i,4:53]
      for(i in FV){
        if(i >= 1){
          # V1: Binary threshold - once passed, always infects, otherwise never infects.
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          p = 0.5*(tanh(0.015*(i-200))+1)
          q = runif(1) # A random number to compare to our infection chance.
          if(q <= p){
            nt = nt + 1
          }
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    GRL100PIMHMList = rbind(GRL100PIMHMList, NHM)
  }
}
GRL100PIMHMList = data.frame(GRL100PIMHMList)
names(GRL100PIMHMList) = c('k', 'pb', 'MeanR0')

GRL100PIMPlot <- ggplot(data = GRL100PIMHMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# I want to plot the original version too so we can compare them.
dat = read.csv('GRL100HDNR.csv',header = FALSE, sep = ',')
GRL100HMList = c()
for(k in KVec){
  for(pb in PBVec){
    KPBDat = subset(dat, dat$V1 == k)
    KPBDat = subset(KPBDat, KPBDat$V2 == pb)
    R0List = c() # Empty vector to add our Num. Trans. to prior to calculating our mean R0.
    for(i in 1:length(KPBDat$V1)){
      nt = 0
      FV = KPBDat[i,4:53]
      for(i in FV){
        if(i >= 1){
          # V1: Binary threshold - once passed, always infects, otherwise never infects.
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          p = 0.5*(tanh(0.015*(i-200))+1)
          q = runif(1) # A random number to compare to our infection chance.
          if(q <= p){
            nt = nt + 1
          }
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    GRL100HMList = rbind(GRL100HMList, NHM)
  }
}
GRL100HMList = data.frame(GRL100HMList)
names(GRL100HMList) = c('k', 'pb', 'MeanR0')

GRL100HDNRPlot <- ggplot(data = GRL100HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# We combine these into a single plot.
CompPlot <- grid.arrange(grobs = list(GRL100HDNRPlot,GRL100PIMPlot), nrow = 1, ncol = 2)

## Time for a full heatmap, GRL/GNR100/25 grid as in the main text. It should look the same as the original
## but with smaller mean R0 values (since some flies die before they cause infections.)
KVec = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
PBVec = c(1000,5950,10900,15850,20800,25750,30700,35650,40600,45550,50500,55450,60400,65350,70300,75250,80200,85150,90100,95050,100000)

dat = read.csv('GRL100PIM.csv',header = FALSE, sep = ',')
GRL100PIMList = c()
for(k in KVec){
  for(pb in PBVec){
    KPBDat = subset(dat, dat$V1 == k)
    KPBDat = subset(KPBDat, KPBDat$V2 == pb)
    R0List = c() # Empty vector to add our Num. Trans. to prior to calculating our mean R0.
    for(i in 1:length(KPBDat$V1)){
      nt = 0
      FV = KPBDat[i,4:53]
      for(i in FV){
        if(i >= 1){
          # V1: Binary threshold - once passed, always infects, otherwise never infects.
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          p = 0.5*(tanh(0.015*(i-200))+1)
          q = runif(1) # A random number to compare to our infection chance.
          if(q <= p){
            nt = nt + 1
          }
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    GRL100PIMList = rbind(GRL100PIMList, NHM)
  }
}
GRL100PIMList = data.frame(GRL100PIMList)
names(GRL100PIMList) = c('k', 'pb', 'MeanR0')

GRL100PIMPlot <- ggplot(data = GRL100PIMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# We now need a second heatmap, for GNR100HDNR.
dat = read.csv('GNR100PIM.csv',header = FALSE, sep = ',')
GNR100PIMList = c()
for(k in KVec){
  for(pb in PBVec){
    KPBDat = subset(dat, dat$V1 == k)
    KPBDat = subset(KPBDat, KPBDat$V2 == pb)
    R0List = c() # Empty vector to add our Num. Trans. to prior to calculating our mean R0.
    for(i in 1:length(KPBDat$V1)){
      nt = 0
      FV = KPBDat[i,4:53]
      for(i in FV){
        if(i >= 1){
          # V1: Binary threshold - once passed, always infects, otherwise never infects.
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          p = 0.5*(tanh(0.015*(i-200))+1)
          q = runif(1) # A random number to compare to our infection chance.
          if(q <= p){
            nt = nt + 1
          }
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    GNR100PIMList = rbind(GNR100PIMList, NHM)
  }
}
GNR100PIMList = data.frame(GNR100PIMList)
names(GNR100PIMList) = c('k', 'pb', 'MeanR0')

GNR100PIMPlot <- ggplot(data = GNR100PIMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A third for GRL25HDNR
dat = read.csv('GRL25PIM.csv',header = FALSE, sep = ',')
GRL25PIMList = c()
for(k in KVec){
  for(pb in PBVec){
    KPBDat = subset(dat, dat$V1 == k)
    KPBDat = subset(KPBDat, KPBDat$V2 == pb)
    R0List = c() # Empty vector to add our Num. Trans. to prior to calculating our mean R0.
    for(i in 1:length(KPBDat$V1)){
      nt = 0
      FV = KPBDat[i,4:53]
      for(i in FV){
        if(i >= 1){
          # V1: Binary threshold - once passed, always infects, otherwise never infects.
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          p = 0.5*(tanh(0.015*(i-200))+1)
          q = runif(1) # A random number to compare to our infection chance.
          if(q <= p){
            nt = nt + 1
          }
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    GRL25PIMList = rbind(GRL25PIMList, NHM)
  }
}
GRL25PIMList = data.frame(GRL25PIMList)
names(GRL25PIMList) = c('k', 'pb', 'MeanR0')

GRL25PIMPlot <- ggplot(data = GRL25PIMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A fourth for GNR25HDNR.

dat = read.csv('GNR25PIM.csv',header = FALSE, sep = ',')
GNR25PIMList = c()
for(k in KVec){
  for(pb in PBVec){
    KPBDat = subset(dat, dat$V1 == k)
    KPBDat = subset(KPBDat, KPBDat$V2 == pb)
    R0List = c() # Empty vector to add our Num. Trans. to prior to calculating our mean R0.
    for(i in 1:length(KPBDat$V1)){
      nt = 0
      FV = KPBDat[i,4:53]
      for(i in FV){
        if(i >= 1){
          # V1: Binary threshold - once passed, always infects, otherwise never infects.
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          #p = 0.5*(tanh(0.01*(i-500))+1)
          p = 0.5*(tanh(0.015*(i-200))+1)
          q = runif(1) # A random number to compare to our infection chance.
          if(q <= p){
            nt = nt + 1
          }
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    GNR25PIMList = rbind(GNR25PIMList, NHM)
  }
}
GNR25PIMList = data.frame(GNR25PIMList)
names(GNR25PIMList) = c('k', 'pb', 'MeanR0')

GNR25PIMPlot <- ggplot(data = GNR25PIMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# These are to be combined into a 2x2 grid. I'm also annotating a little, so this takes 2 steps.
LeftHalf <- grid.arrange(grobs = list(GNR100PIMPlot, GNR25PIMPlot), nrow = 2, ncol = 1, top = "Model A")
RightHalf <- grid.arrange(grobs = list(GRL100PIMPlot, GRL25PIMPlot), nrow = 2, ncol = 1, top = "Model B")
FullPIMGrid <- grid.arrange(grobs = list(LeftHalf, RightHalf), nrow = 1, ncol = 2)


## Replicating fig 4c for a couple of alternative scenarios. The first with no carry cap, the second with pop sinks.
# Vector of max lifespans.
MLVec = c(5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30)
"Now Processing RAG 1"
dat = read.csv('MaxLSVarNoCCDatM1.csv',header = FALSE, sep = ',')
"Loaded M1 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MNCC1Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MNCC1Array = rbind(MNCC1Array,RAVec)
}
MNCC1Array = data.frame(MNCC1Array)
names(MNCC1Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 4"
dat = read.csv('MaxLSVarNoCCDatM4.csv',header = FALSE, sep = ',')
"Loaded M4 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MNCC4Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MNCC4Array = rbind(MNCC4Array,RAVec)
}
MNCC4Array = data.frame(MNCC4Array)
names(MNCC4Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 7"
dat = read.csv('MaxLSVarNoCCDatM7.csv',header = FALSE, sep = ',')
"Loaded M7 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MNCC7Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MNCC7Array = rbind(MNCC7Array,RAVec)
}
MNCC7Array = data.frame(MNCC7Array)
names(MNCC7Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 13"
dat = read.csv('MaxLSVarNoCCDatM13.csv',header = FALSE, sep = ',')
"Loaded M13 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MNCC13Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MNCC13Array = rbind(MNCC13Array,RAVec)
}
MNCC13Array = data.frame(MNCC13Array)
names(MNCC13Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 18"
dat = read.csv('MaxLSVarNoCCDatM18.csv',header = FALSE, sep = ',')
"Loaded M18 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MNCC18Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MNCC18Array = rbind(MNCC18Array,RAVec)
}
MNCC18Array = data.frame(MNCC18Array)
names(MNCC18Array) = c('MaxLifespan', 'MeanR0')

"Mouse Prcessing Completed"
# This gives us a set of arrays contained Mean R0 vs Max Lifespan. We plot each as a line on a line plot.
# Hopefully we will see them increase in value as the max lifespan increases - sharply between 15 and 20 days,
# permitting that the mice lie in the correct regions. We can check that with the heatmaps.

MaxLifespanLinesNCC <- ggplot()+
  geom_point(data = MNCC1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = MNCC4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = MNCC7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = MNCC13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = MNCC18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value")

# Next, the pop sink version.
"Now Processing RAG 1"
dat = read.csv('MaxLSVarSinksDatM1.csv',header = FALSE, sep = ',')
"Loaded M1 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MS1Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MS1Array = rbind(MS1Array,RAVec)
}
MS1Array = data.frame(MS1Array)
names(MS1Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 4"
dat = read.csv('MaxLSVarSinksDatM4.csv',header = FALSE, sep = ',')
"Loaded M4 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MS4Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MS4Array = rbind(MS4Array,RAVec)
}
MS4Array = data.frame(MS4Array)
names(MS4Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 7"
dat = read.csv('MaxLSVarSinksDatM7.csv',header = FALSE, sep = ',')
"Loaded M7 Dataset"
# NOTE: dat$V7 is irrelevant now, but the bite vector still starts at V5.
MS7Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MS7Array = rbind(MS7Array,RAVec)
}
MS7Array = data.frame(MS7Array)
names(MS7Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 13"
dat = read.csv('MaxLSVarSinksDatM13.csv',header = FALSE, sep = ',')
"Loaded M13 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MS13Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MS13Array = rbind(MS13Array,RAVec)
}
MS13Array = data.frame(MS13Array)
names(MS13Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 18"
dat = read.csv('MaxLSVarSinksDatM18.csv',header = FALSE, sep = ',')
"Loaded M18 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MS18Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MS18Array = rbind(MS18Array,RAVec)
}
MS18Array = data.frame(MS18Array)
names(MS18Array) = c('MaxLifespan', 'MeanR0')

"Mouse Processing Completed"
# This gives us a set of arrays contained Mean R0 vs Max Lifespan. We plot each as a line on a line plot.
# Hopefully we will see them increase in value as the max lifespan increases - sharply between 15 and 20 days,
# permitting that the mice lie in the correct regions. We can check that with the heatmaps.

MaxLifespanLinesS <- ggplot()+
  geom_point(data = MS1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = MS4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = MS7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = MS13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = MS18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value")

# Omitting some code for now. It exists within FinalGraphCollection.R...

MaxLifespanLinesDefault <- ggplot()+
  geom_point(data = M1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = M4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = M7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = M13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = M18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value")

LineGraphComparison <- grid.arrange(grobs = list(MaxLifespanLinesNCC, MaxLifespanLinesS, MaxLifespanLinesDefault), nrow = 3, ncol = 1)


## Another line plot, this time for the larger sinks.
# Next, the pop sink version.
"Now Processing RAG 1"
dat = read.csv('MaxLSVarLargeSinksDatM1.csv',header = FALSE, sep = ',')
"Loaded M1 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MLS1Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MLS1Array = rbind(MLS1Array,RAVec)
}
MLS1Array = data.frame(MLS1Array)
names(MLS1Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 4"
dat = read.csv('MaxLSVarLargeSinksDatM4.csv',header = FALSE, sep = ',')
"Loaded M4 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MLS4Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MLS4Array = rbind(MLS4Array,RAVec)
}
MLS4Array = data.frame(MLS4Array)
names(MLS4Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 7"
dat = read.csv('MaxLSVarLargeSinksDatM7.csv',header = FALSE, sep = ',')
"Loaded M7 Dataset"
# NOTE: dat$V7 is irrelevant now, but the bite vector still starts at V5.
MLS7Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MLS7Array = rbind(MLS7Array,RAVec)
}
MLS7Array = data.frame(MLS7Array)
names(MLS7Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 13"
dat = read.csv('MaxLSVarLargeSinksDatM13.csv',header = FALSE, sep = ',')
"Loaded M13 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MLS13Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MLS13Array = rbind(MLS13Array,RAVec)
}
MLS13Array = data.frame(MLS13Array)
names(MLS13Array) = c('MaxLifespan', 'MeanR0')

"Now Processing RAG 18"
dat = read.csv('MaxLSVarLargeSinksDatM18.csv',header = FALSE, sep = ',')
"Loaded M18 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
MLS18Array = c()
for(ML in MLVec){
  datsub = subset(dat,dat$V1 == ML)
  R0Vec = c()
  for(i in 1:length(datsub$V1)){
    NB = datsub[i,2]
    if(NB > 0){
      BVec = datsub[i,5:5+(NB-1)]
      TVec = lapply(BVec,R0Calc)
      R0 = 0
      for(j in TVec){
        if(is.null(j) == FALSE){
          R0 = R0 + 1
        }
      }
      R0Vec = rbind(R0Vec,R0)
    }
    else{
      R0 = 0
      R0Vec = rbind(R0Vec,R0)
    }
  }
  MR0 = mean(R0Vec)
  RAVec = c(ML,MR0)
  MLS18Array = rbind(MLS18Array,RAVec)
}
MLS18Array = data.frame(MLS18Array)
names(MLS18Array) = c('MaxLifespan', 'MeanR0')

"Mouse Processing Completed"
# This gives us a set of arrays contained Mean R0 vs Max Lifespan. We plot each as a line on a line plot.
# Hopefully we will see them increase in value as the max lifespan increases - sharply between 15 and 20 days,
# permitting that the mice lie in the correct regions. We can check that with the heatmaps.
# Please NOTE: if you have not ran the code for Fig 4c in FinalGraphCollection, it will not plot here!
# I do not duplicate the code for brevity.

MaxLifespanLinesDefault <- ggplot()+
  geom_point(data = M1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = M4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = M7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = M13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = M18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value", limits = c(0,0.35))

MaxLifespanLinesNCC <- ggplot()+
  geom_point(data = MNCC1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = MNCC4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = MNCC7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = MNCC13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = MNCC18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value", limits = c(0,0.35))

MaxLifespanLinesS <- ggplot()+
  geom_point(data = MS1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = MS4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = MS7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = MS13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = MS18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value", limits = c(0,0.35))

MaxLifespanLinesLS <- ggplot()+
  geom_point(data = MLS1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = MLS4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = MLS7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = MLS13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = MLS18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value", limits = c(0,0.35))

# Plotting the line graphs as a grid, with annotations.
QuarterA = grid.arrange(grobs = list(MaxLifespanLinesDefault), nrow = 1, ncol = 1, top = "A")
QuarterB = grid.arrange(grobs = list(MaxLifespanLinesNCC), nrow = 1, ncol = 1, top = "B")
QuarterC = grid.arrange(grobs = list(MaxLifespanLinesS), nrow = 1, ncol = 1, top = "C")
QuarterD = grid.arrange(grobs = list(MaxLifespanLinesLS), nrow = 1, ncol = 1, top = "D")
FullLineGrid = grid.arrange(grobs = list(QuarterA, QuarterB, QuarterC, QuarterD), nrow = 2, ncol = 2)

