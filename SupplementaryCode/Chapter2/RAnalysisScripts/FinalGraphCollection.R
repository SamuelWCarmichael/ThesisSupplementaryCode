# This is the script used to produce at the 'final' images for the main document.
# Other images are used within the supplementary materials, however.
# NOTE: Certain components involve large files and can take a long time to run!

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/TomMastersStuff/MyCode/RScriptsAnalysis')

KVec = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
PBVec = c(1000,5950,10900,15850,20800,25750,30700,35650,40600,45550,50500,55450,60400,65350,70300,75250,80200,85150,90100,95050,100000)

# Fig 3: Heatmap Grid, Smooth Threshold. Binary option is present in the supplementary version, but also avaliable here.

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

# A third for GRL25HDNR
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

# A fourth for GNR25HDNR.

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

# These are to be combined into a 2x2 grid. I'm also annotating a little, so this takes 2 steps.
LeftHalf <- grid.arrange(grobs = list(GNR100HDNRPlot, GNR25HDNRPlot), nrow = 2, ncol = 1, top = "Model A")
RightHalf <- grid.arrange(grobs = list(GRL100HDNRPlot, GRL25HDNRPlot), nrow = 2, ncol = 1, top = "Model B")
FullGrid <- grid.arrange(grobs = list(LeftHalf, RightHalf), nrow = 1, ncol = 2)

# Fig 2: Combined Seafim/Doehl rep graph. Yes, this came after Fig 3, but it doesn't really matter.

# Next the Serafim/Doehl rep combined graph.
SRDat = read.csv('SROutFinal.csv',header = TRUE,sep = ',', na.strings = "NA")
SRDatRV = read.csv('SROutFinalRP.csv',header = TRUE,sep = ',', na.strings = "NA")

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

SRDat1B= subset(SRDat, SRDat$NumBites == 1)
SRDat2B= subset(SRDat, SRDat$NumBites == 2)
SRDatRV1B= subset(SRDatRV, SRDatRV$NumBites == 1)
SRDatRV2B= subset(SRDatRV, SRDatRV$NumBites == 2)

# First we plot the Serafim graphs individually and then combine them.
MetaNoG <- ggplot()+
  geom_jitter(data = SRDat, aes(x = "02", y = SRDat$Day2Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "03", y = SRDat$Day3Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "06", y = SRDat$Day6Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "09", y = SRDat$Day9Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "12a", y = SRDat1B$Day12Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "12b", y = SRDat2B$Day12Metas, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "13a", y = SRDat1B$Day13Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "13b", y = SRDat2B$Day13Metas, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "14a", y = SRDat1B$Day14Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "14b", y = SRDat2B$Day14Metas, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "15a", y = SRDat1B$Day15Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "15b", y = SRDat2B$Day15Metas, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "18a", y = SRDat1B$Day18Metas, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "18b", y = SRDat2B$Day18Metas, colour = "Two Bites"), height = 0.0, shape  = 18)+
  scale_colour_manual(name="Point Color", values=c("One Bite"="#86b3df", "Two Bites"="#e57e1a"))+
  #xlab("Days Post-infection")+
  ylab("Number of Metacyclics")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank())


# Next, the retroleptomonads.

RetroNoG <- ggplot()+
  geom_jitter(data = SRDat, aes(x = "02", y = SRDat$Day2Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "03", y = SRDat$Day3Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "06", y = SRDat$Day6Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat, aes(x = "09", y = SRDat$Day9Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "12a", y = SRDat1B$Day12Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "12b", y = SRDat2B$Day12Retros, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "13a", y = SRDat1B$Day13Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "13b", y = SRDat2B$Day13Retros, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "14a", y = SRDat1B$Day14Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "14b", y = SRDat2B$Day14Retros, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "15a", y = SRDat1B$Day15Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "15b", y = SRDat2B$Day15Retros, colour = "Two Bites"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat1B, aes(x = "18a", y = SRDat1B$Day18Retros, colour = "One Bite"), height = 0.0, shape  = 18)+
  geom_jitter(data = SRDat2B, aes(x = "18b", y = SRDat2B$Day18Retros, colour = "Two Bites"), height = 0.0, shape  = 18)+
  scale_colour_manual(name="Point Color", values=c("One Bite"="#86b3df", "Two Bites"="#e57e1a"))+
  xlab("Days Post-infection")+
  ylab("Number of Retroleptomonads")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

SerafimRepHalf <- grid.arrange(grobs = list(MetaNoG,RetroNoG), top = "A")

# Next we plot the Doehl rep graph individually.

MouseRepDat = read.csv('DoehlMouseReplicator.csv',header = FALSE, sep = ',')
MouseVec = c(1,2,3,4,5,6,7,8,9)
thresh = 500
MOutput = c()

for(N in 9:18){
  MDat = subset(MouseRepDat, MouseRepDat$V3 == N)
  paraVec = MDat$V4
  total = length(paraVec)
  numInf = 0
  for(i in paraVec){
    if(i >= thresh){
      numInf = numInf + 1
    }
  }
  propInf = numInf/total
  NHM = c(N)
  NHM = cbind(NHM, numInf)
  MOutput = rbind(MOutput,NHM)
}

MOutput = data.frame(MOutput)
names(MOutput) = c('Mouse','numInf')

DoehlRepGraph <- ggplot(data = MOutput, aes(x = as.factor(Mouse), y = numInf))+
  geom_bar(stat = "identity", fill = "#86b3df")+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(name = "RAG Mouse")+
  scale_y_continuous(name = "Number of Infected Sandflies")

DoehlRepHalf <- grid.arrange(grobs = list(DoehlRepGraph), ncol = 1, nrow = 1, top = "B")

# Now we can merge these together.
ReplicationGridFull <- grid.arrange(grobs = list(SerafimRepHalf, DoehlRepHalf), nrow = 1, ncol = 2)

# Fig 4: A combination of heatmaps and a line graph investigating max lifespan and transmission.
# Once, these were separate, and they take a while to run. Would definitely be faster to swap to lapply rather than loops.
# First the 20 day max lifespan.
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
          #if(i >= trThr){
          #  nt = nt + 1
          #}
          # V2: Tanh based threshold function.
          #p = 0.5*(tanh(0.01*(i-500))+1) # Out of date line, use the second one!
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
    HMList = rbind(HMList, NHM)
  }
}
HMList = data.frame(HMList)
names(HMList) = c('k', 'pb', 'MeanR0')

MVec = c(1*10^3.2,1.2697)
MVec = rbind(MVec, c(1*10^3.1,0.9156))
MVec = rbind(MVec, c(1*10^3.2,1.2474))
MVec = rbind(MVec, c(1*10^3.0,0.9523))
MVec = rbind(MVec, c(1*10^4.0,0.7229))
MVec = rbind(MVec, c(1*10^5.0,0.6561))
MVec = rbind(MVec, c(1*10^5.0,0.8124))
MVec = rbind(MVec, c(1*10^4.0,0.7222))
MVec = rbind(MVec, c(1.057*10^4,0.9081))
MVec = rbind(MVec, c(6.2977*10^3,0.7723))
MVec = rbind(MVec, c(9.242*10^3,1.1523))
MVec = rbind(MVec, c(6.7356*10^4,1.2146))
MVec = rbind(MVec, c(2.1384*10^4,0.5017))
MVec = rbind(MVec, c(1.6032*10^4,1.2758))
MVec = rbind(MVec, c(9.315*10^4,1.6440))
MVec = data.frame(MVec)
names(MVec) = c('pb','k')

# This has prepared the data such that we can plot two different heatmaps. I shall do so now:
# First: Mean R0 Proxy NOTE: Add limits to the scale_fill part to change the z-scale.

D20Heatmap <- ggplot(data = HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  geom_point(data = MVec, aes(x = k, y = pb), shape = 4, size = 2.5)+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.1)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

# Then the 15 day max lifespan.
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
    HMList = rbind(HMList, NHM)
  }
}
HMList = data.frame(HMList)
names(HMList) = c('k', 'pb', 'MeanR0')

MVec = c(1*10^3.2,1.2697)
MVec = rbind(MVec, c(1*10^3.1,0.9156))
MVec = rbind(MVec, c(1*10^3.2,1.2474))
MVec = rbind(MVec, c(1*10^3.0,0.9523))
MVec = rbind(MVec, c(1*10^4.0,0.7229))
MVec = rbind(MVec, c(1*10^5.0,0.6561))
MVec = rbind(MVec, c(1*10^5.0,0.8124))
MVec = rbind(MVec, c(1*10^4.0,0.7222))
MVec = rbind(MVec, c(1.057*10^4,0.9081))
MVec = rbind(MVec, c(6.2977*10^3,0.7723))
MVec = rbind(MVec, c(9.242*10^3,1.1523))
MVec = rbind(MVec, c(6.7356*10^4,1.2146))
MVec = rbind(MVec, c(2.1384*10^4,0.5017))
MVec = rbind(MVec, c(1.6032*10^4,1.2758))
MVec = rbind(MVec, c(9.315*10^4,1.6440))
MVec = data.frame(MVec)
names(MVec) = c('pb','k')

# This has prepared the data such that we can plot two different heatmaps. I shall do so now:
# First: Mean R0 Proxy NOTE: Add limits to the scale_fill part to change the z-scale.

D15Heatmap <- ggplot(data = HMList, aes(x = k, y = pb, fill = MeanR0))+
  geom_tile()+
  geom_point(data = MVec, aes(x = k, y = pb), shape = 4, size = 2.5)+
  #xlab('k Value')+
  #ylab('Mean Skin Parasite Burden')+
  scale_fill_distiller(name = "Mean R0",palette = "RdYlBu", limits = c(0.0,0.02)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Skin Homogeneity", breaks = KVec, limits = NULL)+
  scale_y_continuous(name = "Mean Skin Parasite Burden",breaks = PBVec,limits = NULL)

D20Graph <- grid.arrange(grobs = list(D20Heatmap), nrow = 1, ncol = 1, top = "A")
D15Graph <- grid.arrange(grobs = list(D15Heatmap), nrow = 1, ncol = 1, top = "B")
HeatmapHalf <- grid.arrange(grobs = list(D20Graph, D15Graph), nrow = 1, ncol = 2)

# Now we plot the max lifespan line graph to combine with these.

MLVec = c(5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30)
RMVec = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18) # Not really used now in the new format.

# going to define a function entitled the R0 Calculator:
R0Calc <- function(N){
  NT = 0
  p = 0
  if(N > 1){
    p = 0.5*(tanh(0.015*(N-200))+1)
  }
  q = runif(1)
  if(q < p){
    NT = 1
  }
}
# Though this is a long section, the smaller files make it much more efficient than the prior part.
# Again, there are likely further optimisations possible but it is not too bad.
# Doing mice one at a time: RAG 1.
"Now Processing RAG 1"
dat = read.csv('MaxLSVarDatM1.csv',header = FALSE, sep = ',')
"Loaded M1 Dataset"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M1Array = c()
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
  M1Array = rbind(M1Array,RAVec)
}
M1Array = data.frame(M1Array)
names(M1Array) = c('MaxLifespan', 'MeanR0')

# RAG 2
"Now pocessing RAG 2"
dat = read.csv('MaxLSVarDatM2.csv',header = FALSE, sep = ',')
"M2 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M2Array = c()
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
  M2Array = rbind(M2Array,RAVec)
}
M2Array = data.frame(M2Array)
names(M2Array) = c('MaxLifespan', 'MeanR0')

# RAG 3
"Now Processing RAG 3"
dat = read.csv('MaxLSVarDatM3.csv',header = FALSE, sep = ',')
"M3 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M3Array = c()
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
  M3Array = rbind(M3Array,RAVec)
}
M3Array = data.frame(M3Array)
names(M3Array) = c('MaxLifespan', 'MeanR0')

# RAG 4
"Now Processing RAG 4"
dat = read.csv('MaxLSVarDatM4.csv',header = FALSE, sep = ',')
"M4 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M4Array = c()
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
  M4Array = rbind(M4Array,RAVec)
}
M4Array = data.frame(M4Array)
names(M4Array) = c('MaxLifespan', 'MeanR0')

# RAG 5
"Now Processing RAG 5"
dat = read.csv('MaxLSVarDatM5.csv',header = FALSE, sep = ',')
"M5 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M5Array = c()
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
  M5Array = rbind(M5Array,RAVec)
}
M5Array = data.frame(M5Array)
names(M5Array) = c('MaxLifespan', 'MeanR0')

# RAG 6
"Now Processing RAG 6"
dat = read.csv('MaxLSVarDatM6.csv',header = FALSE, sep = ',')
"M6 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M6Array = c()
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
  M6Array = rbind(M6Array,RAVec)
}
M6Array = data.frame(M6Array)
names(M6Array) = c('MaxLifespan', 'MeanR0')

# RAG 7
"Now Processing RAG 7"
dat = read.csv('MaxLSVarDatM7.csv',header = FALSE, sep = ',')
"M7 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M7Array = c()
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
  M7Array = rbind(M7Array,RAVec)
}
M7Array = data.frame(M7Array)
names(M7Array) = c('MaxLifespan', 'MeanR0')

# RAG 8
"Now Processing RAG 8"
dat = read.csv('MaxLSVarDatM8.csv',header = FALSE, sep = ',')
"M8 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M8Array = c()
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
  M8Array = rbind(M8Array,RAVec)
}
M8Array = data.frame(M8Array)
names(M8Array) = c('MaxLifespan', 'MeanR0')

# RAG 9
"Now Processing RAG 9"
dat = read.csv('MaxLSVarDatM9.csv',header = FALSE, sep = ',')
"M9 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M9Array = c()
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
  M9Array = rbind(M9Array,RAVec)
}
M9Array = data.frame(M9Array)
names(M9Array) = c('MaxLifespan', 'MeanR0')

# RAG 10
"Now Processing RAG 10"
dat = read.csv('MaxLSVarDatM10.csv',header = FALSE, sep = ',')
"M10 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M10Array = c()
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
  M10Array = rbind(M10Array,RAVec)
}
M10Array = data.frame(M10Array)
names(M10Array) = c('MaxLifespan', 'MeanR0')

# RAG 11
"Now Processing RAG 11"
dat = read.csv('MaxLSVarDatM11.csv',header = FALSE, sep = ',')
"M11 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M11Array = c()
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
  M11Array = rbind(M11Array,RAVec)
}
M11Array = data.frame(M11Array)
names(M11Array) = c('MaxLifespan', 'MeanR0')

# RAG 12
"Now Processing RAG 12"
dat = read.csv('MaxLSVarDatM12.csv',header = FALSE, sep = ',')
"M12 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M12Array = c()
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
  M12Array = rbind(M12Array,RAVec)
}
M12Array = data.frame(M12Array)
names(M12Array) = c('MaxLifespan', 'MeanR0')

# RAG 13
"Now Processing RAG 13"
dat = read.csv('MaxLSVarDatM13.csv',header = FALSE, sep = ',')
"M13 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M13Array = c()
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
  M13Array = rbind(M13Array,RAVec)
}
M13Array = data.frame(M13Array)
names(M13Array) = c('MaxLifespan', 'MeanR0')

# RAG 14
"Now Processing RAG 14"
dat = read.csv('MaxLSVarDatM14.csv',header = FALSE, sep = ',')
"M14 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M14Array = c()
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
  M14Array = rbind(M14Array,RAVec)
}
M14Array = data.frame(M14Array)
names(M14Array) = c('MaxLifespan', 'MeanR0')

# RAG 15
"Now Processing RAG 15"
dat = read.csv('MaxLSVarDatM15.csv',header = FALSE, sep = ',')
"M15 Dataset loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M15Array = c()
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
  M15Array = rbind(M15Array,RAVec)
}
M15Array = data.frame(M15Array)
names(M15Array) = c('MaxLifespan', 'MeanR0')

# RAG 16
"Now Processing RAG 16"
dat = read.csv('MaxLSVarDatM16.csv',header = FALSE, sep = ',')
"M16 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M16Array = c()
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
  M16Array = rbind(M16Array,RAVec)
}
M16Array = data.frame(M16Array)
names(M16Array) = c('MaxLifespan', 'MeanR0')

# RAG 17
"Now Processing RAG 17"
dat = read.csv('MaxLSVarDatM17.csv',header = FALSE, sep = ',')
"M17 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M17Array = c()
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
  M17Array = rbind(M17Array,RAVec)
}
M17Array = data.frame(M17Array)
names(M17Array) = c('MaxLifespan', 'MeanR0')

# RAG 18
"Now Processing RAG 18"
dat = read.csv('MaxLSVarDatM18.csv',header = FALSE, sep = ',')
"M18 Dataset Loaded"
# NOTE: dat$V4 is irrelevant now, but the bite vector still starts at V5.
M18Array = c()
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
  M18Array = rbind(M18Array,RAVec)
}
M18Array = data.frame(M18Array)
names(M18Array) = c('MaxLifespan', 'MeanR0')
"Mouse Prcessing Completed"
# This gives us a set of arrays contained Mean R0 vs Max Lifespan. We plot each as a line on a line plot.
# Hopefully we will see them increase in value as the max lifespan increases - sharply between 15 and 20 days,
# permitting that the mice lie in the correct regions. We can check that with the heatmaps.

ggplot()+
  geom_point(data = M1Array, aes(x = MaxLifespan, y = MeanR0), colour = "blue", shape = 0)+
  geom_point(data = M2Array, aes(x = MaxLifespan, y = MeanR0), colour = "red", shape = 1)+
  geom_point(data = M3Array, aes(x = MaxLifespan, y = MeanR0), colour = "magenta", shape = 2)+
  geom_point(data = M4Array, aes(x = MaxLifespan, y = MeanR0), colour = "cyan", shape = 3)+
  geom_point(data = M5Array, aes(x = MaxLifespan, y = MeanR0), colour = "black", shape = 4)+
  geom_point(data = M6Array, aes(x = MaxLifespan, y = MeanR0), colour = "blue", shape = 5)+
  geom_point(data = M7Array, aes(x = MaxLifespan, y = MeanR0), colour = "red", shape = 6)+
  geom_point(data = M8Array, aes(x = MaxLifespan, y = MeanR0), colour = "magenta", shape = 7)+
  geom_point(data = M9Array, aes(x = MaxLifespan, y = MeanR0), colour = "cyan", shape = 8)+
  geom_point(data = M10Array, aes(x = MaxLifespan, y = MeanR0), colour = "black", shape = 9)+
  geom_point(data = M11Array, aes(x = MaxLifespan, y = MeanR0), colour = "blue", shape = 10)+
  geom_point(data = M12Array, aes(x = MaxLifespan, y = MeanR0), colour = "red", shape = 11)+
  geom_point(data = M13Array, aes(x = MaxLifespan, y = MeanR0), colour = "magenta", shape = 12)+
  geom_point(data = M14Array, aes(x = MaxLifespan, y = MeanR0), colour = "cyan", shape = 13)+
  geom_point(data = M15Array, aes(x = MaxLifespan, y = MeanR0), colour = "black", shape = 14)+
  geom_point(data = M16Array, aes(x = MaxLifespan, y = MeanR0), colour = "blue", shape = 15)+
  geom_point(data = M17Array, aes(x = MaxLifespan, y = MeanR0), colour = "red", shape = 16)+
  geom_point(data = M18Array, aes(x = MaxLifespan, y = MeanR0), colour = "magenta", shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value")


MaxLifespanLines <- ggplot()+
  geom_point(data = M1Array, aes(x = MaxLifespan, y = MeanR0), shape = 0)+
  geom_point(data = M2Array, aes(x = MaxLifespan, y = MeanR0), shape = 1)+
  geom_point(data = M3Array, aes(x = MaxLifespan, y = MeanR0), shape = 2)+
  geom_point(data = M4Array, aes(x = MaxLifespan, y = MeanR0), shape = 3)+
  geom_point(data = M5Array, aes(x = MaxLifespan, y = MeanR0), shape = 4)+
  geom_point(data = M6Array, aes(x = MaxLifespan, y = MeanR0), shape = 5)+
  geom_point(data = M7Array, aes(x = MaxLifespan, y = MeanR0), shape = 6)+
  geom_point(data = M8Array, aes(x = MaxLifespan, y = MeanR0), shape = 7)+
  geom_point(data = M9Array, aes(x = MaxLifespan, y = MeanR0), shape = 8)+
  geom_point(data = M10Array, aes(x = MaxLifespan, y = MeanR0), shape = 9)+
  geom_point(data = M11Array, aes(x = MaxLifespan, y = MeanR0), shape = 10)+
  geom_point(data = M12Array, aes(x = MaxLifespan, y = MeanR0), shape = 11)+
  geom_point(data = M13Array, aes(x = MaxLifespan, y = MeanR0), shape = 12)+
  geom_point(data = M14Array, aes(x = MaxLifespan, y = MeanR0), shape = 13)+
  geom_point(data = M15Array, aes(x = MaxLifespan, y = MeanR0), shape = 14)+
  geom_point(data = M16Array, aes(x = MaxLifespan, y = MeanR0), shape = 15)+
  geom_point(data = M17Array, aes(x = MaxLifespan, y = MeanR0), shape = 16)+
  geom_point(data = M18Array, aes(x = MaxLifespan, y = MeanR0), shape = 17)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(name = "Maximum Lifespan (Days)", breaks = c(5,10,15,20,25,30), limits = c(3,32))+
  scale_y_continuous(name = "Mean R0 Value")

# Time to merge this with the others.
MaxLifespanHalf <- grid.arrange(grobs = list(MaxLifespanLines), nrow = 1, ncol = 1, top = "C")
Graph678Full <- grid.arrange(grobs = list(HeatmapHalf, MaxLifespanHalf), nrow = 2, ncol = 1)