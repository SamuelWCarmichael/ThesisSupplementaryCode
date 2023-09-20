# Supplementary images for Ch3.
# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
library(viridis)
library(igraph)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/NetworkModelSection/ConnectednessR0EstimateSims/R0CompSims/RScripts')

figs1dat = read.csv('HomogUnifTRate015Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
figs1x = c(1:maxgen)
figs1means = c() # Potentially this won't be needed, but just in case.
figs1lws = c() # Lower whiskers, which will be mean - 1se.
figs1uws = c() # Upper whiskers, which will be mean + 1se.
figs1frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(figs1dat,figs1dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  figs1means = c(figs1means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  figs1lws = c(figs1lws,max(lws,0))
  figs1uws = c(figs1uws,uws)
}

figs1frame = data.frame(
  x = figs1x,
  lw = figs1lws,
  mean = figs1means,
  uw = figs1uws
)

# Finally, we calculate the analytic estimate relevant to this run.

figs1anest = (1-(((1-0.01)^41 - (1-0.02)^41)/(41*0.01)))*4 # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

figs1 = ggplot(figs1frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=figs1anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

# S2: Bimodal distr. but with 0.015 transmission rate.

figs2dat = read.csv('HomogBimodTRate015Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
figs2x = c(1:maxgen)
figs2means = c() # Potentially this won't be needed, but just in case.
figs2lws = c() # Lower whiskers, which will be mean - 1se.
figs2uws = c() # Upper whiskers, which will be mean + 1se.
figs2frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(figs2dat,figs2dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  figs2means = c(figs2means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  figs2lws = c(figs2lws,max(lws,0))
  figs2uws = c(figs2uws,uws)
}

figs2frame = data.frame(
  x = figs2x,
  lw = figs2lws,
  mean = figs2means,
  uw = figs2uws
)

# Finally, we calculate the analytic estimate relevant to this run.

figs2anest = (1 - 0.99*(1-0.0061)^40 - 0.01*(1-0.9)^40)*4 # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

figs2 = ggplot(figs2frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=figs2anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()