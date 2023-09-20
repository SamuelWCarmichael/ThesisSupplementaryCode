# An extension of ch3mainplotter, handling some final new plots. These are both related to the epidemic threshold
# work and given that they are somewhat complex I'm isolating them in this script.

library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
library(viridis)
library(igraph)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/NetworkModelSection/ConnectednessR0EstimateSims/R0CompSims/RScripts')

# Community Structured, Full.
fig12abrdat = read.csv('CSAbThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12abdiagdat = read.csv('CSAbThreshFixedTRateProp0DiagDat.csv',header=TRUE)
fig12berdat = read.csv('CSBeThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12bediagdat = read.csv('CSBeThreshFixedTRateProp0DiagDat.csv',header=TRUE)

fig12neabrdat = read.csv('CSNeAbThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12neabdiagdat = read.csv('CSNeAbThreshFixedTRateProp0DiagDat.csv',header=TRUE)
fig12neberdat = read.csv('CSNeBeThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12nebediagdat = read.csv('CSNeBeThreshFixedTRateProp0DiagDat.csv',header=TRUE)
fig12faabrdat = read.csv('CSFaAbThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12faabdiagdat = read.csv('CSFaAbThreshFixedTRateProp0DiagDat.csv',header=TRUE)
fig12faberdat = read.csv('CSFaBeThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12fabediagdat = read.csv('CSFaBeThreshFixedTRateProp0DiagDat.csv',header=TRUE)
fig12vfabrdat = read.csv('CSVFaAbThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12vfabdiagdat = read.csv('CSVFaAbThreshFixedTRateProp0DiagDat.csv',header=TRUE)
fig12vfberdat = read.csv('CSVFaBeThreshFixedTRateProp0RDat.csv',header=TRUE)
fig12vfbediagdat = read.csv('CSVFaBeThreshFixedTRateProp0DiagDat.csv',header=TRUE)

fig13x = c(0.0005,0.002,0.0026,0.0032,0.0044,0.005,0.0056,0.007)
fig13y = c(sum(fig12vfbediagdat$Extinct)/200,sum(fig12fabediagdat$Extinct)/200,sum(fig12bediagdat$Extinct)/200,sum(fig12nebediagdat$Extinct)/200,sum(fig12neabdiagdat$Extinct)/200,sum(fig12abdiagdat$Extinct)/200,sum(fig12faabdiagdat$Extinct)/200,sum(fig12vfabdiagdat$Extinct)/200)

fig13frame = data.frame(
  x = as.factor(fig13x),
  y = fig13y
)

fig13 = ggplot()+
  geom_col(data = fig13frame,aes(x=x,y=y,fill=x))+
  scale_fill_brewer(palette = "RdBu")+
  geom_vline(xintercept = 4.5)+
  ylim(c(0,1))+
  ylab('Proportion Extinct')+
  xlab('Transmission Rate')+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))


f14a = cbind(fig12vfbediagdat$ExtTime,0.0005)
f14b = cbind(fig12fabediagdat$ExtTime,0.002)
f14c = cbind(fig12bediagdat$ExtTime,0.0026)
f14d = cbind(fig12nebediagdat$ExtTime,0.0032)
f14e = cbind(fig12neabdiagdat$ExtTime,0.0044)
f14f = cbind(fig12abdiagdat$ExtTime,0.005)
f14g = cbind(fig12faabdiagdat$ExtTime,0.0056)
f14h = cbind(fig12vfabdiagdat$ExtTime,0.007)

f14 = rbind(f14a,f14b)
f14 = rbind(f14,f14c)
f14 = rbind(f14,f14d)
f14 = rbind(f14,f14e)
f14 = rbind(f14,f14f)
f14 = rbind(f14,f14g)
f14 = rbind(f14,f14h)

f14 = data.frame(f14)
names(f14) = c('ExtTime','TRate')
f14$TRate = as.factor(f14$TRate)

fig14 = ggplot()+
  #geom_violin(data=f14,aes(x=TRate,y=ExtTime,group=TRate))+
  geom_boxplot(data=f14,aes(x=TRate,y=ExtTime,group=TRate),outlier.shape=NA)+
  geom_jitter(data=f14,aes(x=TRate,y=ExtTime,colour=TRate))+
  geom_vline(xintercept = 1.5,color="grey")+
  geom_vline(xintercept = 2.5,color="grey")+
  geom_vline(xintercept = 3.5,color="grey")+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5,color="grey")+
  geom_vline(xintercept = 6.5,color="grey")+
  geom_vline(xintercept = 7.5,color="grey")+
  xlab("Transmission Rate")+
  ylab("Extinction Time")+
  scale_fill_brewer(palette = "RdBu")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig7a = ggplot()+
  geom_col(data = fig13frame,aes(x=x,y=y,fill=x))+
  scale_fill_brewer(palette = "RdBu")+
  geom_vline(xintercept = 4.5)+
  annotate("text",x = 8,y = 0.75,label = "A",size = 10)+
  ylim(c(0,1))+
  ylab('Proportion of Simulations Extinct')+
  xlab('Transmission Rate')+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig7b = ggplot()+
  geom_boxplot(data=f14,aes(x=TRate,y=ExtTime,group=TRate),outlier.shape=NA)+
  geom_jitter(data=f14,aes(x=TRate,y=ExtTime,colour=TRate))+
  geom_vline(xintercept = 1.5,color="grey")+
  geom_vline(xintercept = 2.5,color="grey")+
  geom_vline(xintercept = 3.5,color="grey")+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5,color="grey")+
  geom_vline(xintercept = 6.5,color="grey")+
  geom_vline(xintercept = 7.5,color="grey")+
  annotate("text",x = 8,y = 50,label = "B",size = 10)+
  xlab("Transmission Rate")+
  ylab("Extinction Time")+
  scale_fill_brewer(palette = "RdBu")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig7 = grid.arrange(grobs = list(dualfig7a,dualfig7b), nrow = 2, ncol = 1)

# Community Structured, Random.
fig15abrdat = read.csv('CSAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15abdiagdat = read.csv('CSAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig15berdat = read.csv('CSBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15bediagdat = read.csv('CSBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)

fig15neabrdat = read.csv('CSNeAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15neabdiagdat = read.csv('CSNeAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig15neberdat = read.csv('CSNeBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15nebediagdat = read.csv('CSNeBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig15faabrdat = read.csv('CSFaAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15faabdiagdat = read.csv('CSFaAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig15faberdat = read.csv('CSFaBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15fabediagdat = read.csv('CSFaBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig15vfabrdat = read.csv('CSVFaAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15vfabdiagdat = read.csv('CSVFaAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig15vfberdat = read.csv('CSVFaBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig15vfbediagdat = read.csv('CSVFaBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)

fig15x = c(0.0005,0.002,0.0026,0.0032,0.0044,0.005,0.0056,0.007)
fig15y = c(sum(fig15vfbediagdat$Extinct)/200,sum(fig15fabediagdat$Extinct)/200,sum(fig15bediagdat$Extinct)/200,sum(fig15nebediagdat$Extinct)/200,sum(fig15neabdiagdat$Extinct)/200,sum(fig15abdiagdat$Extinct)/200,sum(fig15faabdiagdat$Extinct)/200,sum(fig15vfabdiagdat$Extinct)/200)

fig15frame = data.frame(
  x = as.factor(fig15x),
  y = fig15y
)

fig15 = ggplot()+
  geom_col(data = fig15frame,aes(x=x,y=y,fill=x))+
  scale_fill_brewer(palette = "RdBu")+
  geom_vline(xintercept = 4.5)+
  ylim(c(0,1))+
  ylab('Proportion Extinct')+
  xlab('Transmission Rate')+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))


f16a = cbind(fig15vfbediagdat$ExtTime,0.0005)
f16b = cbind(fig15fabediagdat$ExtTime,0.002)
f16c = cbind(fig15bediagdat$ExtTime,0.0026)
f16d = cbind(fig15nebediagdat$ExtTime,0.0032)
f16e = cbind(fig15neabdiagdat$ExtTime,0.0044)
f16f = cbind(fig15abdiagdat$ExtTime,0.005)
f16g = cbind(fig15faabdiagdat$ExtTime,0.0056)
f16h = cbind(fig15vfabdiagdat$ExtTime,0.007)

f16 = rbind(f16a,f16b)
f16 = rbind(f16,f16c)
f16 = rbind(f16,f16d)
f16 = rbind(f16,f16e)
f16 = rbind(f16,f16f)
f16 = rbind(f16,f16g)
f16 = rbind(f16,f16h)

f16 = data.frame(f16)
names(f16) = c('ExtTime','TRate')
f16$TRate = as.factor(f16$TRate)

fig16 = ggplot()+
  #geom_violin(data=f14,aes(x=TRate,y=ExtTime,group=TRate))+
  geom_boxplot(data=f16,aes(x=TRate,y=ExtTime,group=TRate),outlier.shape=NA)+
  geom_jitter(data=f16,aes(x=TRate,y=ExtTime,colour=TRate))+
  geom_vline(xintercept = 1.5,color="grey")+
  geom_vline(xintercept = 2.5,color="grey")+
  geom_vline(xintercept = 3.5,color="grey")+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5,color="grey")+
  geom_vline(xintercept = 6.5,color="grey")+
  geom_vline(xintercept = 7.5,color="grey")+
  xlab("Transmission Rate")+
  ylab("Extinction Time")+
  scale_fill_brewer(palette = "RdBu")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig8a = ggplot()+
  geom_col(data = fig15frame,aes(x=x,y=y,fill=x))+
  scale_fill_brewer(palette = "RdBu")+
  geom_vline(xintercept = 4.5)+
  annotate("text",x = 8,y = 0.75,label = "A",size = 10)+
  ylim(c(0,1))+
  ylab('Proportion of Simulations Extinct')+
  xlab('Transmission Rate')+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig8b = ggplot()+
  geom_boxplot(data=f16,aes(x=TRate,y=ExtTime,group=TRate),outlier.shape=NA)+
  geom_jitter(data=f16,aes(x=TRate,y=ExtTime,colour=TRate))+
  geom_vline(xintercept = 1.5,color="grey")+
  geom_vline(xintercept = 2.5,color="grey")+
  geom_vline(xintercept = 3.5,color="grey")+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5,color="grey")+
  geom_vline(xintercept = 6.5,color="grey")+
  geom_vline(xintercept = 7.5,color="grey")+
  annotate("text",x = 8,y = 50,label = "B",size = 10)+
  xlab("Transmission Rate")+
  ylab("Extinction Time")+
  scale_fill_brewer(palette = "RdBu")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig8 = grid.arrange(grobs = list(dualfig8a,dualfig8b), nrow = 2, ncol = 1)
