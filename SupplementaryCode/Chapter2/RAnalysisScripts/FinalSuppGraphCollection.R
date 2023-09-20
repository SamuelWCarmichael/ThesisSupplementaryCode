# This is the script used to produce at the 'final' images for the supplementary materials.
# NOTE: Certain components read large files and thus can take a long time to run!

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes)
library(gridExtra)
# Set my working directory. Mostly for ease of use, can be done manually, of course.
setwd('~/LeishmaniaPhDStuff/TomMastersStuff/MyCode/RScriptsAnalysis')

# S1 code
# a litle more setup needed. We have a couple of relevant files to load and alter.
SRDat = read.csv('SROutFinal.csv',header = TRUE,sep = ',', na.strings = "NA") 
SRDatRV = read.csv('SROutFinalRP.csv',header = TRUE,sep = ',', na.strings = "NA") # Data with the parameters randomised. Needed for a later graph.
# This is to neaten the graphs. NA is assumed to be 1, which isn't true. We don't wish to plot NA, so we set it to 3 instead.
# A value of 3 lies off the graph, giving us the desired effect. Probably there are better ways to handle this.
for(i in 1:20000){
  for(j in 1:44){
    if(is.na(SRDat[i,j]) == TRUE){
      SRDat[i,j] = 3.0 # 3.0 so it gets omitted from my plots (NA plots at 1.0, for some reason)
    }
  }
}
for(i in 1:20000){
  for(j in 1:44){
    if(is.na(SRDatRV[i,j]) == TRUE){
      SRDatRV[i,j] = 3.0 # 3.0 so it gets omitted from my plots (NA plots at 1.0, for some reason)
    }
  }
}
# Subsetting in advance. The smaller sets as a result are (slightly) faster to read from.
SRDat1B= subset(SRDat, SRDat$NumBites == 1)
SRDat2B= subset(SRDat, SRDat$NumBites == 2)
SRDatRV1B= subset(SRDatRV, SRDatRV$NumBites == 1)
SRDatRV2B= subset(SRDatRV, SRDatRV$NumBites == 2)

# Now we get to plotting. s1 is the proportions of metas and retros at different days, as per the work of Serafim et al.
# First, the proportions of metacyclics.
MetaPropG <- ggplot()+
  geom_jitter(data = SRDat, aes(x = "02", y = SRDat$Day2MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "03", y = SRDat$Day3MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "06", y = SRDat$Day6MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "09", y = SRDat$Day9MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "12a", y = SRDat1B$Day12MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "12b", y = SRDat2B$Day12MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "13a", y = SRDat1B$Day13MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "13b", y = SRDat2B$Day13MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "14a", y = SRDat1B$Day14MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "14b", y = SRDat2B$Day14MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "15a", y = SRDat1B$Day15MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "15b", y = SRDat2B$Day15MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "18a", y = SRDat1B$Day18MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "18b", y = SRDat2B$Day18MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  scale_colour_manual(name="Point Color", values=c("One Bite"="#86b3df", "Two Bites"="#e57e1a"))+
  #ylim(0,1)+
  #xlab("Days Post-infection")+
  #ylab("Proportion Metacyclics")+
  theme_bw()+
  scale_y_continuous(name = "Proportion Metacyclics (%)",breaks = c(0.0,0.25,0.5,0.75,1.0),labels = c(0.0,25.0,50.0,75.0,100.0),limits = c(0.0,1.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank())

# next, the proportion of retroleptomonads. This will be merged with the previous one eventually into a single panel.

RetroPropG <- ggplot()+
  geom_jitter(data = SRDat, aes(x = "02", y = SRDat$Day2RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "03", y = SRDat$Day3RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "06", y = SRDat$Day6RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "09", y = SRDat$Day9RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "12a", y = SRDat1B$Day12RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "12b", y = SRDat2B$Day12RetroProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "13a", y = SRDat1B$Day13RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "13b", y = SRDat2B$Day13RetroProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "14a", y = SRDat1B$Day14RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "14b", y = SRDat2B$Day14RetroProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "15a", y = SRDat1B$Day15RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "15b", y = SRDat2B$Day15RetroProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "18a", y = SRDat1B$Day18RetroProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "18b", y = SRDat2B$Day18RetroProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  scale_colour_manual(name="Point Color", values=c("One Bite"="#86b3df", "Two Bites"="#e57e1a"))+
  #ylim(0,1)+
  xlab("Days Post-infection")+
  #ylab("Proportion Retroleptomonads")+
  theme_bw()+
  scale_y_continuous(name = "Proportion Retroleptomonads (%)",breaks = c(0.0,0.25,0.5,0.75,1.0),labels = c(0.0,25.0,50.0,75.0,100.0),limits = c(0.0,1.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# I want these combined into a multi-panel figure.
PropComboGraph <- grid.arrange(grobs = list(MetaPropG,RetroPropG))

# S2 Code
# S2 is the proportions of metas with parameter randomisation included.

RandomParamsGraph <- ggplot()+
  geom_jitter(data = SRDatRV, aes(x = "02", y = SRDatRV$Day2MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV, aes(x = "03", y = SRDatRV$Day3MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV, aes(x = "06", y = SRDatRV$Day6MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV, aes(x = "09", y = SRDatRV$Day9MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV1B, aes(x = "12a", y = SRDatRV1B$Day12MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV2B, aes(x = "12b", y = SRDatRV2B$Day12MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV1B, aes(x = "13a", y = SRDatRV1B$Day13MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV2B, aes(x = "13b", y = SRDatRV2B$Day13MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV1B, aes(x = "14a", y = SRDatRV1B$Day14MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV2B, aes(x = "14b", y = SRDatRV2B$Day14MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV1B, aes(x = "15a", y = SRDatRV1B$Day15MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV2B, aes(x = "15b", y = SRDatRV2B$Day15MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV1B, aes(x = "18a", y = SRDatRV1B$Day18MetaProp, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDatRV2B, aes(x = "18b", y = SRDatRV2B$Day18MetaProp, colour = "Two Bites"), height = 0.0, shape  = 18)+
  scale_colour_manual(name="Point Color", values=c("One Bite"="#86b3df", "Two Bites"="#e57e1a"))+
  #ylim(0,1)+
  xlab("Days Post-infection")+
  #ylab("Proportion Metacyclics")+
  theme_bw()+
  scale_y_continuous(name = "Proportion Metacyclics (%)",breaks = c(0.0,0.25,0.5,0.75,1.0),labels = c(0.0,25.0,50.0,75.0,100.0),limits = c(0.0,1.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# S3/S4 Combined Code.
# Each heatmap has 2 possible threshold functions, use V1 to generate S3, and v2 to generate S4.
trThr = 200 # A binary threshold, Estimates vary considerably for this, I'm using 200, others could be chosen.
KVec = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
PBVec = c(1000,5950,10900,15850,20800,25750,30700,35650,40600,45550,50500,55450,60400,65350,70300,75250,80200,85150,90100,95050,100000)

# First graph is for GRL100HDNR.
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

# We now need a second heatmap, for GNR100HDNR.
dat = read.csv('GNR100HDNR.csv',header = FALSE, sep = ',')
GNR100HMList = c()
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
    GNR100HMList = rbind(GNR100HMList, NHM)
  }
}
GNR100HMList = data.frame(GNR100HMList)
names(GNR100HMList) = c('k', 'pb', 'MeanR0')

GNR100HDNRPlot <- ggplot(data = GNR100HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A third for GRL50HDNR

dat = read.csv('GRL50HDNR.csv',header = FALSE, sep = ',')
GRL50HMList = c()
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
    GRL50HMList = rbind(GRL50HMList, NHM)
  }
}
GRL50HMList = data.frame(GRL50HMList)
names(GRL50HMList) = c('k', 'pb', 'MeanR0')

GRL50HDNRPlot <- ggplot(data = GRL50HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A fourth for GNR50HDNR

dat = read.csv('GNR50HDNR.csv',header = FALSE, sep = ',')
GNR50HMList = c()
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
    GNR50HMList = rbind(GNR50HMList, NHM)
  }
}
GNR50HMList = data.frame(GNR50HMList)
names(GNR50HMList) = c('k', 'pb', 'MeanR0')

GNR50HDNRPlot <- ggplot(data = GNR50HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A fifth for GRL25HDNR
dat = read.csv('GRL25HDNR.csv',header = FALSE, sep = ',')
GRL25HMList = c()
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
    GRL25HMList = rbind(GRL25HMList, NHM)
  }
}
GRL25HMList = data.frame(GRL25HMList)
names(GRL25HMList) = c('k', 'pb', 'MeanR0')

GRL25HDNRPlot <- ggplot(data = GRL25HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A sixth for GNR25HDNR.

dat = read.csv('GNR25HDNR.csv',header = FALSE, sep = ',')
GNR25HMList = c()
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
    GNR25HMList = rbind(GNR25HMList, NHM)
  }
}
GNR25HMList = data.frame(GNR25HMList)
names(GNR25HMList) = c('k', 'pb', 'MeanR0')

GNR25HDNRPlot <- ggplot(data = GNR25HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# A seventh for GRL10HDNR

dat = read.csv('GRL10HDNR.csv',header = FALSE, sep = ',')
GRL10HMList = c()
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
    GRL10HMList = rbind(GRL10HMList, NHM)
  }
}
GRL10HMList = data.frame(GRL10HMList)
names(GRL10HMList) = c('k', 'pb', 'MeanR0')

GRL10HDNRPlot <- ggplot(data = GRL10HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.7)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# An eighth for GNR10HDNR

dat = read.csv('GNR10HDNR.csv',header = FALSE, sep = ',')
GNR10HMList = c()
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
    GNR10HMList = rbind(GNR10HMList, NHM)
  }
}
GNR10HMList = data.frame(GNR10HMList)
names(GNR10HMList) = c('k', 'pb', 'MeanR0')

GNR10HDNRPlot <- ggplot(data = GNR10HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)


# These are to be combined into a 8x2 grid. I'm also annotating a little, so this takes 2 steps.
LeftHalf <- grid.arrange(grobs = list(GNR100HDNRPlot, GNR50HDNRPlot, GNR25HDNRPlot, GNR10HDNRPlot), nrow = 4, ncol = 1, top = "Model A")
RightHalf <- grid.arrange(grobs = list(GRL100HDNRPlot, GRL50HDNRPlot, GRL25HDNRPlot, GRL10HDNRPlot), nrow = 4, ncol = 1, top = "Model B")
FullGrid <- grid.arrange(grobs = list(LeftHalf, RightHalf), nrow = 1, ncol = 2)

# S5 Code
# A heatmap of 20-day max old flies, but with the binary threshold not the smooth one. The option to enable the smooth one exists here too.

dat = read.csv('GRLSLS100HD.csv',header = FALSE, sep = ',')

trThr = 200 # Arbitrary binary threshold. To be honest, estimates for this range between a few to a thousand, so I'm using 50.
KVec = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
PBVec = c(1000,5950,10900,15850,20800,25750,30700,35650,40600,45550,50500,55450,60400,65350,70300,75250,80200,85150,90100,95050,100000)
HMList = c()
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
          if(i >= trThr){
            nt = nt + 1
          }
          # V2: Tanh based threshold function.
          #p = 0.5*(tanh(0.015*(i-200))+1)
          #q = runif(1) # A random number to compare to our infection chance.
          #if(q <= p){
          #  nt = nt + 1
          #}
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    HMList = rbind(HMList, NHM)
  }
}
HMList = data.frame(HMList)
names(HMList) = c('k', 'pb', 'MeanR0')

D20Heatmap <- ggplot(data = HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# S6 Code
# The heatmap for 15-day max old flies with the binary threshold not the smooth threshold.

dat = read.csv('GRLVSLS100HD.csv',header = FALSE, sep = ',')

# A little further setup required.
trThr = 200 # Arbitrary binary threshold. To be honest, estimates for this range between a few to a thousand, so I'm using 50.
KVec = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
PBVec = c(1000,5950,10900,15850,20800,25750,30700,35650,40600,45550,50500,55450,60400,65350,70300,75250,80200,85150,90100,95050,100000)
HMList = c()
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
          #V1: Binary threshold - once passed, always infects, otherwise never infects.
          if(i >= trThr){
            nt = nt + 1
          }
          # V2: Tanh based threshold function.
          #p = 0.5*(tanh(0.015*(i-200))+1)
          #q = runif(1) # A random number to compare to our infection chance.
          #if(q <= p){
          #  nt = nt + 1
          #}
        }
      }
      R0List = rbind(R0List,nt)
    }
    # We now a list of the num. trans. by all flies in this set.
    MeanR0 = mean(R0List)
    NHM = c(k)
    NHM = cbind(NHM, pb)
    NHM = cbind(NHM, MeanR0)
    HMList = rbind(HMList, NHM)
  }
}
HMList = data.frame(HMList)
names(HMList) = c('k', 'pb', 'MeanR0')

D15Heatmap <- ggplot(data = HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.02)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)
