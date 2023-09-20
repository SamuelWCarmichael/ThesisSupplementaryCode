# This is the main plotting script for chapter 3. It holds all the figures, supplementary or otherwise.
# It also handles all data processing needed for that.

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
library(viridis)
library(igraph)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/NetworkModelSection/ConnectednessR0EstimateSims/R0CompSims/RScripts')

###############################################################################

    # Part 1: Discrete Timesteps #

###############################################################################

# First, we can calculate the analytic estimates for the simulations (which are equivalent for
# both random/structured sims). We use these as a comparison with the numerical output.

# For this, we need the diagnostic output. Since all the simulations follow the same degree
# distribution, our mean/var k shouldn't differ between them too much.

csdiagdat = read.csv('DiscUnifDiagDatProp0.csv',header=TRUE) # This is for the community structure distribution.
geomdiagdat = read.csv('DebugFixedTRate03Prop1DiagDat.csv',header=TRUE) # Geometric dist. degrees.
homogdiagdat = read.csv('HomogUnifTRateProp1DiagDat.csv',header=TRUE) # Completely homogeneous, not really needed but here anyway.

cskh = mean(csdiagdat$kh) # Mean k for the comm struct. sims.
cskhh = mean(csdiagdat$kh2) # Var k for the comm struct. sims.

geokh = mean(geomdiagdat$kh) # Mean k for the geometric dist. sims.
geokhh = mean(geomdiagdat$kh2) # Var k for the geometric dist. sims.

homkh = 5 # We know these because it is configured to be perfectly homogeneous, all individuals have degree 5.
homkhh = 0 # And naturally this means no variance.

# For ease of extraction, I want to produce these in the order they are needed in the document.

# First, the boring control sets. A homogeneous network where all individuals have degree 5. The very first set is
# even more boring: transmission rates are fixed to a specific value for all connections. This set allows us to
# demonstrate that the calculations for T work in cases with homogeneity.

homogfixed03rdat = read.csv('HomogFixedTRate03Prop1RDat.csv',header=TRUE)
homogfixed015rdat = read.csv('HomogFixedTRate015Prop1RDat.csv',header=TRUE)

# I want a line plot with the standard deviation of R0 values shown as whiskers. Also a horizontal line that
# indicates the analytic estimate. Only the first 8 or 9 generations will be plotted because the number of higher
# gen individuals begins to diminish and can cause problems for the plotting.

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig1x = c(1:maxgen)
fig1means = c() # Potentially this won't be needed, but just in case.
fig1lws = c() # Lower whiskers, which will be mean - 1se.
fig1uws = c() # Upper whiskers, which will be mean + 1se.
fig1frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(homogfixed03rdat,homogfixed03rdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig1means = c(fig1means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig1lws = c(fig1lws,max(lws,0))
  fig1uws = c(fig1uws,uws)
}

fig1frame = data.frame(
  x = fig1x,
  lw = fig1lws,
  mean = fig1means,
  uw = fig1uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig1anest = (1-(1-0.03)^40)*4

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig1 = ggplot(fig1frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig1anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

### Next, the same plot but for the lower transmission rate.

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig2x = c(1:maxgen)
fig2means = c() # Potentially this won't be needed, but just in case.
fig2lws = c() # Lower whiskers, which will be mean - 1se.
fig2uws = c() # Upper whiskers, which will be mean + 1se.
fig2frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(homogfixed015rdat,homogfixed015rdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig2means = c(fig2means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig2lws = c(fig2lws,max(lws,0))
  fig2uws = c(fig2uws,uws)
}

fig2frame = data.frame(
  x = fig2x,
  lw = fig2lws,
  mean = fig2means,
  uw = fig2uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig2anest = (1-(1-0.015)^40)*4 # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig2 = ggplot(fig2frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig2anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

### We see similar behaviour in these plots (though the estimate behaves differently for gen 1). We will mostly
### include the higher transmission rate figures in the main results, and possibly the lower rate ones in supplementaries.
### We will want to see the counterpart plots for the bimod/unif distributions in this bog-standard context,
### mostly so that we know that the formula for T is working as intended (or at least well enough).

fig3dat = read.csv('HomogUnifTRate03Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig3x = c(1:maxgen)
fig3means = c() # Potentially this won't be needed, but just in case.
fig3lws = c() # Lower whiskers, which will be mean - 1se.
fig3uws = c() # Upper whiskers, which will be mean + 1se.
fig3frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig3dat,fig3dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig3means = c(fig3means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig3lws = c(fig3lws,max(lws,0))
  fig3uws = c(fig3uws,uws)
}

fig3frame = data.frame(
  x = fig3x,
  lw = fig3lws,
  mean = fig3means,
  uw = fig3uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig3anest = (1-(((1-0.02)^41 - (1-0.04)^41)/(41*0.02)))*4 # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig3 = ggplot(fig3frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig3anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

### Next, the bimodal version. We might plot the lower transmission rate version in the supplementary file.

fig4dat = read.csv('HomogBimodTRate03Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig4x = c(1:maxgen)
fig4means = c() # Potentially this won't be needed, but just in case.
fig4lws = c() # Lower whiskers, which will be mean - 1se.
fig4uws = c() # Upper whiskers, which will be mean + 1se.
fig4frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig4dat,fig4dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig4means = c(fig4means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig4lws = c(fig4lws,max(lws,0))
  fig4uws = c(fig4uws,uws)
}

fig4frame = data.frame(
  x = fig4x,
  lw = fig4lws,
  mean = fig4means,
  uw = fig4uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig4anest = (1 - 0.98*(1-0.0122)^40 - 0.02*(1-0.9)^40)*4 # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig4 = ggplot(fig4frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig4anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

### So, we show that the estimate for T performs reasonably, at least for the first generation or two. We expect this.
### Next we examine the simpler degree distribution: the geometrically distributed degrees. This is picked because
### it is a distribution with a known formula for mean/variance. I do not linger here for long, though.

fig5dat = read.csv('DebugFixedTRate03Prop1RDat.csv',header=TRUE)
fig5diagdat = read.csv('DebugFixedTRate03Prop1DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig5x = c(1:maxgen)
fig5means = c() # Potentially this won't be needed, but just in case.
fig5lws = c() # Lower whiskers, which will be mean - 1se.
fig5uws = c() # Upper whiskers, which will be mean + 1se.
fig5frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig5dat,fig5dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig5means = c(fig5means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig5lws = c(fig5lws,max(lws,0))
  fig5uws = c(fig5uws,uws)
}

fig5frame = data.frame(
  x = fig5x,
  lw = fig5lws,
  mean = fig5means,
  uw = fig5uws
)

# Finally, we calculate the analytic estimate relevant to this run.

p = 0.2577
#gmn = (1-p)/p
#gvr = (1-p)/p^2

gmn = mean(fig5diagdat$kh)
gvr = mean(fig5diagdat$kh2)

fig5anest = (1-(1-0.03)^40)*(gmn - 1 + gvr/gmn)
#fig5anest = (1-(1-0.03)^40)*(gmn - 1)

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig5 = ggplot(fig5frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig5anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

### Hmm... quite the overestimate. It appears completely wrong. It works fine with the homogeneous estimator though.
### Let's also examine the non-fixed transmission.

fig6dat = read.csv('DebugUnifTRate03Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig6x = c(1:maxgen)
fig6means = c() # Potentially this won't be needed, but just in case.
fig6lws = c() # Lower whiskers, which will be mean - 1se.
fig6uws = c() # Upper whiskers, which will be mean + 1se.
fig6frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig6dat,fig6dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig6means = c(fig6means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig6lws = c(fig6lws,max(lws,0))
  fig6uws = c(fig6uws,uws)
}

fig6frame = data.frame(
  x = fig6x,
  lw = fig6lws,
  mean = fig6means,
  uw = fig6uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig6anest = (1-(((1-0.02)^41 - (1-0.04)^41)/(41*0.02)))*(gmn - 1 + gvr/gmn) # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig6 = ggplot(fig6frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig6anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

### Bimodal distr.

fig7dat = read.csv('DebugBimodTRate03Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig7x = c(1:maxgen)
fig7means = c() # Potentially this won't be needed, but just in case.
fig7lws = c() # Lower whiskers, which will be mean - 1se.
fig7uws = c() # Upper whiskers, which will be mean + 1se.
fig7frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig7dat,fig7dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig7means = c(fig7means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig7lws = c(fig7lws,max(lws,0))
  fig7uws = c(fig7uws,uws)
}

fig7frame = data.frame(
  x = fig7x,
  lw = fig7lws,
  mean = fig7means,
  uw = fig7uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig7anest = (1 - 0.98*(1-0.0122)^40 - 0.02*(1-0.9)^40)*(gmn - 1 + gvr/gmn) # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig7 = ggplot(fig7frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig7anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

### So, completely wrong. Interesting. As a final check, I've made an 'unweighted' version. We do not match stubs
### in constructing these networks, instead we pair individuals. Not my preferred approach, might make a difference.
### I want it ruled out either way before I draw conclusions.

fig8dat = read.csv('DebugUWFixedTRate03Prop1RDat.csv',header=TRUE)
fig8diagdat = read.csv('DebugUWFixedTRate03Prop1DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig8x = c(1:maxgen)
fig8means = c() # Potentially this won't be needed, but just in case.
fig8lws = c() # Lower whiskers, which will be mean - 1se.
fig8uws = c() # Upper whiskers, which will be mean + 1se.
fig8frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig8dat,fig8dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig8means = c(fig8means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig8lws = c(fig8lws,max(lws,0))
  fig8uws = c(fig8uws,uws)
}

fig8frame = data.frame(
  x = fig8x,
  lw = fig8lws,
  mean = fig8means,
  uw = fig8uws
)

# Finally, we calculate the analytic estimate relevant to this run.

p = 0.2049
#gmn = (1-p)/p
#gvr = (1-p)/p^2

gmn = mean(fig8diagdat$kh)
gvr = mean(fig8diagdat$kh2)

fig8anest = (1-(1-0.03)^40)*(gmn - 1 + gvr/gmn)
#fig5anest = (1-(1-0.03)^40)*(gmn - 1)

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig8 = ggplot(fig8frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig8anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

### On to the comm struct. sims. It appears that the estimate is actually rubbish for predicting the R0 value,
### even for gen 1 cases. We should see this here too (indeed, I may run these before the geometric sims).

fig9dat = read.csv('FixedTRate03Prop1RDat.csv',header=TRUE)
fig9diagdat = read.csv('FixedTRate03Prop1DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig9x = c(1:maxgen)
fig9means = c() # Potentially this won't be needed, but just in case.
fig9lws = c() # Lower whiskers, which will be mean - 1se.
fig9uws = c() # Upper whiskers, which will be mean + 1se.
fig9frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig9dat,fig9dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig9means = c(fig9means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig9lws = c(fig9lws,max(lws,0))
  fig9uws = c(fig9uws,uws)
}

fig9frame = data.frame(
  x = fig9x,
  lw = fig9lws,
  mean = fig9means,
  uw = fig9uws
)

# Finally, we calculate the analytic estimate relevant to this run.

gmn = mean(fig9diagdat$kh)
gvr = mean(fig9diagdat$kh2)

fig9anest = (1-(1-0.03)^40)*(gmn - 1 + gvr/gmn)
# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig9 = ggplot(fig9frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig9anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

### As rubbish as anticipated. Curiously the homogeneous estimator works better, too. Strange.

fig10dat = read.csv('UnifTRate03Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig10x = c(1:maxgen)
fig10means = c() # Potentially this won't be needed, but just in case.
fig10lws = c() # Lower whiskers, which will be mean - 1se.
fig10uws = c() # Upper whiskers, which will be mean + 1se.
fig10frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig10dat,fig10dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig10means = c(fig10means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig10lws = c(fig10lws,max(lws,0))
  fig10uws = c(fig10uws,uws)
}

fig10frame = data.frame(
  x = fig10x,
  lw = fig10lws,
  mean = fig10means,
  uw = fig10uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig10anest = (1-(((1-0.02)^41 - (1-0.04)^41)/(41*0.02)))*(gmn - 1 + gvr/gmn) # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig10 = ggplot(fig10frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig10anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

### Finally the bimodal sims.

fig11dat = read.csv('BimodTRate03Prop1RDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
fig11x = c(1:maxgen)
fig11means = c() # Potentially this won't be needed, but just in case.
fig11lws = c() # Lower whiskers, which will be mean - 1se.
fig11uws = c() # Upper whiskers, which will be mean + 1se.
fig11frame = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig11dat,fig11dat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig11means = c(fig11means,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig11lws = c(fig11lws,max(lws,0))
  fig11uws = c(fig11uws,uws)
}

fig11frame = data.frame(
  x = fig11x,
  lw = fig11lws,
  mean = fig11means,
  uw = fig11uws
)

# Finally, we calculate the analytic estimate relevant to this run.

fig11anest = (1 - 0.98*(1-0.0122)^40 - 0.02*(1-0.9)^40)*(gmn - 1 + gvr/gmn) # Not 5 because most individuals will have an infected neighbour (the one who infected them).

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

fig11 = ggplot(fig11frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig11anest,color="#FF0000")+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

### So. One final thing to test is to compare the performance of the system at either side of an epidemic threshold
### predicted using the R0 estimate. This is to evaluate if has any lingering potential in predicting such a thing.
### Inspired by comments made by Lloyd and Valeika.

fig12abrdat = read.csv('DebugAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12abdiagdat = read.csv('DebugAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig12berdat = read.csv('DebugBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12bediagdat = read.csv('DebugBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)

maxgen = 6 # The maximum generation to consider. Avoids the sparser higher generations.

fig12abx = c(1:maxgen)
fig12abmeans = c() # Potentially this won't be needed, but just in case.
fig12ablws = c() # Lower whiskers, which will be mean - 1se.
fig12abuws = c() # Upper whiskers, which will be mean + 1se.
fig12abframe = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig12abrdat,fig12abrdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig12abmeans = c(fig12abmeans,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig12ablws = c(fig12ablws,max(lws,0))
  fig12abuws = c(fig12abuws,uws)
}

fig12abframe = data.frame(
  x = fig12abx,
  lw = fig12ablws,
  mean = fig12abmeans,
  uw = fig12abuws
)

fig12bex = c(1:maxgen)
fig12bemeans = c() # Potentially this won't be needed, but just in case.
fig12belws = c() # Lower whiskers, which will be mean - 1se.
fig12beuws = c() # Upper whiskers, which will be mean + 1se.
fig12beframe = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(fig12berdat,fig12berdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  fig12bemeans = c(fig12bemeans,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  fig12belws = c(fig12belws,max(lws,0))
  fig12beuws = c(fig12beuws,uws)
}

fig12beframe = data.frame(
  x = fig12bex,
  lw = fig12belws,
  mean = fig12bemeans,
  uw = fig12beuws
)

fig12 = ggplot()+
  geom_line(data = fig12abframe,aes(x=x,y = mean),color = "#FF0000")+
  geom_boxplot(data = fig12abframe,aes(x=x,ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity",color = "#FF0000")+
  geom_point(data = fig12abframe,aes(x=x,y=mean),shape = 4,size=5)+
  geom_line(data = fig12beframe,aes(x=x,y = mean),color = "#0000FF")+
  geom_boxplot(data = fig12beframe,aes(x=x,ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity",color = "#0000FF")+
  geom_point(data = fig12beframe,aes(x=x,y=mean),shape = 4,size=5)+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()

# Funnily enough, we the R increases after gen 1. Probably due to the weighted random used to pair stubs.
# But also of interest is data regarding how they went extinct. Probably of more interest than this last figure, really.
# We have 4 sets above and below the threshold, and I want to examine the proportion of extinct simulations and the
# distribution of extinction times.

fig12neabrdat = read.csv('DebugNeAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12neabdiagdat = read.csv('DebugNeAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig12neberdat = read.csv('DebugNeBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12nebediagdat = read.csv('DebugNeBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig12faabrdat = read.csv('DebugFaAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12faabdiagdat = read.csv('DebugFaAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig12faberdat = read.csv('DebugFaBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12fabediagdat = read.csv('DebugFaBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig12vfabrdat = read.csv('DebugVFaAbThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12vfabdiagdat = read.csv('DebugVFaAbThreshFixedTRateProp1DiagDat.csv',header=TRUE)
fig12vfberdat = read.csv('DebugVFaBeThreshFixedTRateProp1RDat.csv',header=TRUE)
fig12vfbediagdat = read.csv('DebugVFaBeThreshFixedTRateProp1DiagDat.csv',header=TRUE)

fig13x = c(0.0015,0.003,0.0036,0.0042,0.0054,0.006,0.0066,0.008)
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

# Not exactly what I had in mind, formatting-wise. Going to try a conventional barplot.
# EDIT: No need, fixed the original!
# f13a = cbind(fig12vfbediagdat$Extinct,0.001)
# f13b = cbind(fig12fabediagdat$Extinct,0.0021)
# f13c = cbind(fig12bediagdat$Extinct,0.00275)
# f13d = cbind(fig12nebediagdat$Extinct,0.0033)
# f13e = cbind(fig12neabdiagdat$Extinct,0.0036)
# f13f = cbind(fig12abdiagdat$Extinct,0.004)
# f13g = cbind(fig12faabdiagdat$Extinct,0.00465)
# f13h = cbind(fig12vfabdiagdat$Extinct,0.006)
# 
# f13 = rbind(f13a,f13b)
# f13 = rbind(f13,f13c)
# f13 = rbind(f13,f13d)
# f13 = rbind(f13,f13e)
# f13 = rbind(f13,f13f)
# f13 = rbind(f13,f13g)
# f13 = rbind(f13,f13h)
# 
# f13 = data.frame(f13)
# names(f13) = c('Extinct','TRate')
# f13$TRate = as.factor(f13$TRate)
# f13 = subset(f13,f13$Extinct>0)
# 
# ggplot()+
#   geom_bar(data=f13,aes(x=TRate,group=Extinct))

### Finally, I want to plot the distributions of extinction times.

f14a = cbind(fig12vfbediagdat$ExtTime,0.0015)
f14b = cbind(fig12fabediagdat$ExtTime,0.003)
f14c = cbind(fig12bediagdat$ExtTime,0.0036)
f14d = cbind(fig12nebediagdat$ExtTime,0.0042)
f14e = cbind(fig12neabdiagdat$ExtTime,0.0054)
f14f = cbind(fig12abdiagdat$ExtTime,0.006)
f14g = cbind(fig12faabdiagdat$ExtTime,0.0066)
f14h = cbind(fig12vfabdiagdat$ExtTime,0.008)

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

#### This is a load of recreated figures that combine previous figures together.

## First, the high/low transmission rates for the fixed trate sims (homogeneous network).

dualfig1a = ggplot(fig1frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig1anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 3.1,label = "A",size = 10)+
  ylim(c(0,3.25))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig1b = ggplot(fig2frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig2anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 3.1,label = "B",size = 10)+
  ylim(c(0,3.25))+
  #ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig1 = grid.arrange(grobs = list(dualfig1a,dualfig1b), nrow = 1, ncol = 2)

## Next is a combination of the uniform and bimodal distr. plots for the higher transmission rate (and avergae rate here).

dualfig2a = ggplot(fig3frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig3anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 3.1,label = "A",size = 10)+
  ylim(c(0,3.25))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig2b = ggplot(fig4frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig4anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 3.1,label = "B",size = 10)+
  ylim(c(0,3.25))+
  #ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig2 = grid.arrange(grobs = list(dualfig2a,dualfig2b), nrow = 1, ncol = 2)

## Now, on to the community structure sims. Originally I wanted to scale things up, but in fact I feel it works better
## this way around: we show how bad it is, then posit the idea it might be the distribution, and show it is still bad
## for a simpler setup... but that we can still use the estimator for one final purpose. Might be changed later.

dualfig3a = ggplot(fig9frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig9anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "A",size = 10)+
  ylim(c(0,5))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

# Huh, never generated this half. Doing so now.

figdf3bdat = read.csv('FixedTRate015Prop1RDat.csv',header=TRUE)
figdf3bdiagdat = read.csv('FixedTRate015Prop1DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
figdf3bx = c(1:maxgen)
figdf3bmeans = c() # Potentially this won't be needed, but just in case.
figdf3blws = c() # Lower whiskers, which will be mean - 1se.
figdf3buws = c() # Upper whiskers, which will be mean + 1se.
figdf3bframe = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(figdf3bdat,figdf3bdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  figdf3bmeans = c(figdf3bmeans,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  figdf3blws = c(figdf3blws,max(lws,0))
  figdf3buws = c(figdf3buws,uws)
}

figdf3bframe = data.frame(
  x = figdf3bx,
  lw = figdf3blws,
  mean = figdf3bmeans,
  uw = figdf3buws
)

# Finally, we calculate the analytic estimate relevant to this run.

gmn = mean(figdf3bdiagdat$kh)
gvr = mean(figdf3bdiagdat$kh2)

figdf3banest = (1-(1-0.015)^40)*(gmn - 1 + gvr/gmn)
# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

dualfig3b = ggplot(figdf3bframe, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=figdf3banest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "B",size = 10)+
  ylim(c(0,5))+
  #ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig3 = grid.arrange(grobs = list(dualfig3a,dualfig3b), nrow = 1, ncol = 2)

## Next, the unif/bimod figure.

dualfig4a = ggplot(fig10frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig10anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "A",size = 10)+
  ylim(c(0,5))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig4b = ggplot(fig11frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig11anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "B",size = 10)+
  ylim(c(0,5))+
  #ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig4 = grid.arrange(grobs = list(dualfig4a,dualfig4b), nrow = 1, ncol = 2)

## Next up, the geometric network figures.

dualfig5a = ggplot(fig5frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig5anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "A",size = 10)+
  ylim(c(0,5))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

figdf5bdat = read.csv('DebugFixedTRate015Prop1RDat.csv',header=TRUE)
figdf5bdiagdat = read.csv('DebugFixedTRate015Prop1DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
figdf5bx = c(1:maxgen)
figdf5bmeans = c() # Potentially this won't be needed, but just in case.
figdf5blws = c() # Lower whiskers, which will be mean - 1se.
figdf5buws = c() # Upper whiskers, which will be mean + 1se.
figdf5bframe = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(figdf5bdat,figdf5bdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  figdf5bmeans = c(figdf5bmeans,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  figdf5blws = c(figdf5blws,max(lws,0))
  figdf5buws = c(figdf5buws,uws)
}

figdf5bframe = data.frame(
  x = figdf5bx,
  lw = figdf5blws,
  mean = figdf5bmeans,
  uw = figdf5buws
)

# Finally, we calculate the analytic estimate relevant to this run.

p = 0.2049
#gmn = (1-p)/p
#gvr = (1-p)/p^2

gmn = mean(figdf5bdiagdat$kh)
gvr = mean(figdf5bdiagdat$kh2)

figdf5banest = (1-(1-0.015)^40)*(gmn - 1 + gvr/gmn)
#fig5anest = (1-(1-0.03)^40)*(gmn - 1)

# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

dualfig5b = ggplot(figdf5bframe, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=figdf5banest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "B",size = 10)+
  ylim(c(0,5))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig5 = grid.arrange(grobs = list(dualfig5a,dualfig5b), nrow = 1, ncol = 2)

### Next, the bimodal/uniform distr. versions. For 0.03 only, see supplementaries for counterparts.

dualfig6a = ggplot(fig6frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig6anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "A",size = 10)+
  ylim(c(0,5))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

dualfig6b = ggplot(fig7frame, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=fig7anest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "B",size = 10)+
  ylim(c(0,5))+
  #ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig6 = grid.arrange(grobs = list(dualfig6a,dualfig6b), nrow = 1, ncol = 2)

### Finally, a vertical multipanel figure of 13/14. It might not be needed, but we'll see.

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

### Bonus version of the comm. struct. sims for the fully random (prop 1) setup.

figdf8adat = read.csv('FixedTRate03Prop0RDat.csv',header=TRUE)
figdf8adiagdat = read.csv('FixedTRate03Prop0DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
figdf8ax = c(1:maxgen)
figdf8ameans = c() # Potentially this won't be needed, but just in case.
figdf8alws = c() # Lower whiskers, which will be mean - 1se.
figdf8auws = c() # Upper whiskers, which will be mean + 1se.
figdf8aframe = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(figdf8adat,figdf8adat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  figdf8ameans = c(figdf8ameans,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  figdf8alws = c(figdf8alws,max(lws,0))
  figdf8auws = c(figdf8auws,uws)
}

figdf8aframe = data.frame(
  x = figdf8ax,
  lw = figdf8alws,
  mean = figdf8ameans,
  uw = figdf8auws
)

# Finally, we calculate the analytic estimate relevant to this run.

gmn = mean(figdf8adiagdat$kh)
gvr = mean(figdf8adiagdat$kh2)

figdf8aanest = (1-(1-0.03)^40)*(gmn - 1 + gvr/gmn)
# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

dualfig8a = ggplot(figdf8aframe, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=figdf8aanest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "A",size = 10)+
  ylim(c(0,5.25))+
  ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15))

figdf8bdat = read.csv('FixedTRate015Prop0RDat.csv',header=TRUE)
figdf8bdiagdat = read.csv('FixedTRate015Prop0DiagDat.csv',header=TRUE)

maxgen = 8 # The maximum generation to consider. Avoids the sparser higher generations.
figdf8bx = c(1:maxgen)
figdf8bmeans = c() # Potentially this won't be needed, but just in case.
figdf8blws = c() # Lower whiskers, which will be mean - 1se.
figdf8buws = c() # Upper whiskers, which will be mean + 1se.
figdf8bframe = c() # A dataframe that will contain all my data for plotting.
for(i in 1:maxgen){
  repvec = c()
  sub = subset(figdf8bdat,figdf8bdat$Gen==i)
  for(j in 1:max(sub$Rep)){
    subsub = subset(sub,sub$Rep==j)
    repent = mean(subsub$SecInfs)
    if(!is.nan(repent)){
      repvec = c(repvec,repent)
    }
  }
  figdf8bmeans = c(figdf8bmeans,mean(repvec))
  lws = mean(repvec) - sqrt(var(repvec))
  uws = mean(repvec) + sqrt(var(repvec))
  figdf8blws = c(figdf8blws,max(lws,0))
  figdf8buws = c(figdf8buws,uws)
}

figdf8bframe = data.frame(
  x = figdf8bx,
  lw = figdf8blws,
  mean = figdf8bmeans,
  uw = figdf8buws
)

# Finally, we calculate the analytic estimate relevant to this run.

gmn = mean(figdf8bdiagdat$kh)
gvr = mean(figdf8bdiagdat$kh2)

figdf8banest = (1-(1-0.015)^40)*(gmn - 1 + gvr/gmn)
# Now, we plot. I'm actually kind of cheating in this plot, but it is a method that should work.

dualfig8b = ggplot(figdf8bframe, aes(x))+
  geom_line(aes(y = mean))+
  geom_boxplot(aes(ymin = lw, lower = mean, middle = mean, upper = mean, ymax = uw,group = x),width=0.1,stat="identity")+
  geom_point(aes(y=mean),shape = 4,size=5)+
  geom_hline(yintercept=figdf8banest,color="#FF0000")+
  annotate("text",x = 7.5,y = 4.5,label = "B",size = 10)+
  ylim(c(0,5.25))+
  #ylab('Secondary Infections Caused')+
  xlab('Generation')+
  theme_classic()+
  theme(axis.title  = element_text(size = 15), axis.text = element_text(size = 15), axis.title.y = element_blank(), axis.text.y = element_blank())

dualfig8 = grid.arrange(grobs = list(dualfig8a,dualfig8b), nrow = 1, ncol = 2)

############### Another new section, Sample Networks! ##########################

netdat1 = read.csv('homogsampleadjmat.csv',header=FALSE)
netdat1 = as.matrix(netdat1)

fig1net <- graph_from_adjacency_matrix(netdat1, mode = "undirected")

plot.igraph(fig1net,vertex.size=5,vertex.label=NA)

netdat2 = read.csv('geometsampleadjmat.csv',header=FALSE)
netdat2 = as.matrix(netdat2)

fig2net <- graph_from_adjacency_matrix(netdat2, mode = "undirected")

plot.igraph(fig2net,vertex.size=5,vertex.label=NA)

netdat3 = read.csv('commstructp0sampleadjmat.csv',header=FALSE)
netdat3 = as.matrix(netdat3)

fig3net <- graph_from_adjacency_matrix(netdat3, mode = "undirected")

plot.igraph(fig3net,vertex.size=5,vertex.label=NA)

netdat4 = read.csv('commstructp1sampleadjmat.csv',header=FALSE)
netdat4 = as.matrix(netdat4)

fig4net <- graph_from_adjacency_matrix(netdat4, mode = "undirected")

plot.igraph(fig4net,vertex.size=5,vertex.label=NA)