# This script is a simple plotter that handles my network samples. It cannot combine them into one figure, however.

# The usual library stuff. Pretty bog standard, really.
library(ggplot2)
library(MASS)
library(hrbrthemes) # Not strictly needed, but useful if different colour schemes are required.
library(gridExtra)
library(viridis)
library(igraph)
# Set my working directory.
setwd('~/LeishmaniaPhDStuff/NetworkModelSection/ConnectednessR0EstimateSims/AnPredLimits/RScripts')

dat1 = read.csv('minpropstrsample.csv',header=FALSE)
dat1 = as.matrix(dat1)

fig1net <- graph_from_adjacency_matrix(dat1, mode = "undirected")

plot.igraph(fig1net,vertex.size=5,vertex.label=NA)

dat2 = read.csv('midpropstrsample.csv',header=FALSE)
dat2 = as.matrix(dat2)

fig2net = graph_from_adjacency_matrix(dat2, mode = "undirected")

plot.igraph(fig2net,vertex.size=5,vertex.label=NA)

dat3 = read.csv('maxpropstrsample.csv',header=FALSE)
dat3 = as.matrix(dat3)

fig3net = graph_from_adjacency_matrix(dat3, mode = "undirected")

plot.igraph(fig3net,vertex.size=5,vertex.label=NA)

dat4 = read.csv('lowpropstrsample.csv',header=FALSE)
dat4 = as.matrix(dat4)

fig4net = graph_from_adjacency_matrix(dat4, mode = "undirected")

plot.igraph(fig4net,vertex.size=5,vertex.label=NA)