% This script uses ode45 to solve the SCPW system from a variety of
% different input parameters, and calculate a second approximation for the
% endemic equilibrium.

global kh alpha beta delta

% MATLAB hates globals, but I... aren't sure I can work around them
% properly. Probably only by meddling with the ode45 input.

% This first section is not recorded, mostly just a preliminary plot.

kh = 4;
kh2 = 17;
kh3 = 76;

alpha = (kh2^2 - kh*kh3)/(kh2 - kh^2);
beta = (kh3 - kh2*kh)/(kh2 - kh^2) - 1;
tau = 0.3;
gam = 1.0;
delta = tau/gam;

y0 = [0.05,0.95,0.05,0.05,0.9];
[tt,yt] = ode45(@SCPWimplem,0:0.1:1000,y0);

% We want to run ode45 for a set of different tau values, such that we move
% our bifurcation parameter away from the critical value (at which our
% transcritical bifurcation occurs). We then extract the 'exact' location
% of the endemic equilibrium and save it for plotting in RStudio.

eeqout = [];

% For simplicity, we will set gam = 1. That way delta = tau and I can
% simplify my calculations.
gam = 1;
for i = 0.3:0.02:0.6
    tau = i;
    delta = tau/gam;
    y0 = [0.05,0.95,0.05,0.05,0.9];
    [t,y] = ode45(@SCPWimplem,0:0.1:100,y0);
    ent = [i,y(end,2)];
    eeqout = [eeqout;ent];
end

tabout = array2table(eeqout,'VariableNames',{'Gamma','InfProp'});
writetable(tabout,'bimodconfode45.csv')

% We will generate a few others too. A function would be more elegant but
% this produces output quickly so regenerating everything isn't too big an
% issue this time.

% Next we repeat for the Poisson dist.

kh = 10;
kh2 = 110;
kh3 = 1309;

alpha = (kh2^2 - kh*kh3)/(kh2 - kh^2);
beta = (kh3 - kh2*kh)/(kh2 - kh^2) - 1;

eeqout2 = [];

% For simplicity, we will set gam = 1. That way delta = tau and I can
% simplify my calculations.
gam = 1;
for i = 0.1:0.01:0.2
    tau = i;
    delta = tau/gam;
    y0 = [0.05,0.95,0.05,0.05,0.9];
    [t,y] = ode45(@SCPWimplem,0:0.1:100,y0);
    ent = [i,y(end,2)];
    eeqout2 = [eeqout2;ent];
end

tabout2 = array2table(eeqout2,'VariableNames',{'Gamma','InfProp'});
writetable(tabout2,'poisconfode45.csv')

% Next, the negative binomial non-config model... not that the config
% status actually changes the calculations here.

kh = 5;
kh2 = 30;
kh3 = 207;

alpha = (kh2^2 - kh*kh3)/(kh2 - kh^2);
beta = (kh3 - kh2*kh)/(kh2 - kh^2) - 1;

eeqout3 = [];

% For simplicity, we will set gam = 1. That way delta = tau and I can
% simplify my calculations.
gam = 1;
for i = 1.2:0.02:2.0
    tau = i;
    delta = tau/gam;
    y0 = [0.05,0.95,0.05,0.05,0.9];
    [t,y] = ode45(@SCPWimplem,0:0.1:100,y0);
    ent = [i,y(end,2)];
    eeqout3 = [eeqout3;ent];
end

tabout3 = array2table(eeqout3,'VariableNames',{'Gamma','InfProp'});
writetable(tabout3,'negbinomode45.csv')

% Next, my own non-config bimodal version.

kh = 4;
kh2 = 20;
kh3 = 112;

alpha = (kh2^2 - kh*kh3)/(kh2 - kh^2);
beta = (kh3 - kh2*kh)/(kh2 - kh^2) - 1;

eeqout4 = [];

% For simplicity, we will set gam = 1. That way delta = tau and I can
% simplify my calculations.
gam = 1;
for i = 1.2:0.02:2.0
    tau = i;
    delta = tau/gam;
    y0 = [0.05,0.95,0.05,0.05,0.9];
    [t,y] = ode45(@SCPWimplem,0:0.1:100,y0);
    ent = [i,y(end,2)];
    eeqout4 = [eeqout4;ent];
end

tabout4 = array2table(eeqout4,'VariableNames',{'Gamma','InfProp'});
writetable(tabout4,'bimodncode45.csv')

% Next, the bipartite, with a smaller version of the negative binomial.

kh = 3.5;
kh2 = 15.4;
kh3 = 80.5;

alpha = (kh2^2 - kh*kh3)/(kh2 - kh^2);
beta = (kh3 - kh2*kh)/(kh2 - kh^2) - 1;

eeqout5 = [];

% For simplicity, we will set gam = 1. That way delta = tau and I can
% simplify my calculations.
gam = 1;
for i = 1.2:0.02:2.0
    tau = i;
    delta = tau/gam;
    y0 = [0.05,0.95,0.05,0.05,0.9];
    [t,y] = ode45(@SCPWimplem,0:0.1:100,y0);
    ent = [i,y(end,2)];
    eeqout5 = [eeqout5;ent];
end

tabout5 = array2table(eeqout5,'VariableNames',{'Gamma','InfProp'});
writetable(tabout5,'bipartode45.csv')
