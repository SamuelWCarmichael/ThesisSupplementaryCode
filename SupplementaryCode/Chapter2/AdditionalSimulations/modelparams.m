% This holds all the parameter values used throughout my code - should be
% ran before the rest of the code is used.

global alpha r s u g q v cc meanLS meanBiteFreq transLow transHigh ...
    transThreshold retroDevT k paraBurd bloodMV infHostChance ...
    loadThreshold biteScaleParam biteChance maxLifespan lifespanReduction lossRate

% Growth/Differentiation Rates
alpha = 1.5189; % Rate of differentiation (Nectomonads -> Leptomonads)
r = 1.4508; % Leptomonad replication rate.
s = 1.6521; % Rate of differentiation (Leptomonads -> Metacyclics)
u = 1.6144; % Rate of metacyclic decline
g = 4.0; % Rate of differentiation (Metacyclics -> Retroleptomonads) Max is 1.5.
q = 3.5; % Retroleptomonad replication rate.
v = 4.0; % Rate of differentiation (Retroleptomonads -> Metacyclics) Max is 1.5.
cc = 2*10^6; % Arbitrarily chosen carry cap of the fly - never seem to have more than ~1.2x10^6 parasites.

% Other Model Parameters

meanLS = 13; % Mean lifespan of the sandfly.
meanBiteFreq = 12; % This is the mean time between bites in days.
biteScaleParam = 0.5; % Other component of the gamma distribution.
transLow = 0.0015; % Proportion of parasites transmitted for hosts with low parasite loads.
transHigh = 0.14; % Proportion of parasites transmitted for hosts with high parasite loads.
transThreshold = 500; % If more parasites than this transferred in a bite, consider it a transmission.
loadThreshold = 1000; % If more than 1000 parasite present, fly has high parasite load.
retroDevT = 4; % Time in which metacyclics dedifferentiate.
k = 2.0; % Dispersion factor: k = 2 is homogeneous, k = 0.3 is heterogeneous.
paraBurd = 10^5; % Average parasites per mm^3 of host skin.
bloodMV = 1.6*10^-3; % Blood meal volume.
infHostChance = 1.0; % Chance of biting a host - I use 0.01, 0.1, 0.25, 0.5 or 1.0.
biteChance = 0.25;
maxLifespan = 50;
lifespanReduction = 0.2; % A newer component - the proportion of remaining lifespan lost when infected.
lossRate = 0.05; % A small loss rate is added to some model forms, this is the important parameter.