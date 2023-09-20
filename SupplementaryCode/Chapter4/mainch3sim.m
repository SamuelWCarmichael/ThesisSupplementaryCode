% This is actually the looping data generator now! Runs off
% discr0compsimrep.m primarily. We want to have a set of replicates for
% different prob. distr. for r, so we can compare how the r0 varies in
% time (well, not really and r0, but near enough).

% We will try different prop values, but I don't think we need the whole
% spectrum this time. Just a structured vs. unstructured comparison will
% do, mostly to show that the R0 doesn't work as well if structured.

%% Part 1: Basic fixed transmission rate (the boring sets).

% We'll consider 2 different prop. values and maybe 2 or 3 different rates,
% just for completeness. We may or may not include them in the final piece.

% Prop = 1 (Fully Random)

[infout,diagout,rIout] = discr0compsimrep(0,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'FixedTRateProp1TSDat.csv')
writetable(diagtab,'FixedTRateProp1DiagDat.csv')
writetable(rItab,'FixedTRateProp1RDat.csv')

% Prop = 0 (Fully Structured)
[infout,diagout,rIout] = discr0compsimrep(0,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'FixedTRateProp0TSDat.csv')
writetable(diagtab,'FixedTRateProp0DiagDat.csv')
writetable(rItab,'FixedTRateProp0RDat.csv')

%% Part 2: Uniformly distributed transmission rates. Same story as part 1.

% Prop = 1 (Fully Random)

[infout,diagout,rIout] = discr0compsimrep(1,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'UnifTRateProp1TSDat.csv')
writetable(diagtab,'UnifTRateProp1DiagDat.csv')
writetable(rItab,'UnifTRateProp1RDat.csv')

% Prop = 0 (Fully Structured)
[infout,diagout,rIout] = discr0compsimrep(1,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'UnifTRateProp0TSDat.csv')
writetable(diagtab,'UnifTRateProp0DiagDat.csv')
writetable(rItab,'UnifTRateProp0RDat.csv')

%% Part 3: Bimodal transmission rates.

% Prop = 1 (Fully Random)

[infout,diagout,rIout] = discr0compsimrep(2,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'BimodTRateProp1TSDat.csv')
writetable(diagtab,'BimodTRateProp1DiagDat.csv')
writetable(rItab,'BimodTRateProp1RDat.csv')

% Prop = 0 (Fully Structured)

[infout,diagout,rIout] = discr0compsimrep(2,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'BimodTRateProp0TSDat.csv')
writetable(diagtab,'BimodTRateProp0DiagDat.csv')
writetable(rItab,'BimodTRateProp0RDat.csv')

%% Part 4: Fixed transmission rate, continuous time.

% Prop = 1 (Fully Random)

[infout,diagout,rIout] = contr0compsimrep(0,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CTFixedTRateProp1TSDat.csv')
writetable(diagtab,'CTFixedTRateProp1DiagDat.csv')
writetable(rItab,'CTFixedTRateProp1RDat.csv')

% Prop = 0 (Fully Structured)
[infout,diagout,rIout] = contr0compsimrep(0,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CTFixedTRateProp0TSDat.csv')
writetable(diagtab,'CTFixedTRateProp0DiagDat.csv')
writetable(rItab,'CTFixedTRateProp0RDat.csv')

%% Part 5: Uniformly distr. transmission rates, continuous time.

% Prop = 1 (Fully Random)

[infout,diagout,rIout] = contr0compsimrep(1,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CTUnifTRateProp1TSDat.csv')
writetable(diagtab,'CTUnifTRateProp1DiagDat.csv')
writetable(rItab,'CTUnifTRateProp1RDat.csv')

% Prop = 0 (Fully Structured)
[infout,diagout,rIout] = contr0compsimrep(1,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CTUnifTRateProp0TSDat.csv')
writetable(diagtab,'CTUnifTRateProp0DiagDat.csv')
writetable(rItab,'CTUnifTRateProp0RDat.csv')

%% Part 6: Bimodally distr. tranmission, continuous time.

% Prop = 1 (Fully Random)

[infout,diagout,rIout] = contr0compsimrep(2,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CTBimodTRateProp1TSDat.csv')
writetable(diagtab,'CTBimodTRateProp1DiagDat.csv')
writetable(rItab,'CTBimodTRateProp1RDat.csv')

% Prop = 0 (Fully Structured)

[infout,diagout,rIout] = contr0compsimrep(2,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CTBimodTRateProp0TSDat.csv')
writetable(diagtab,'CTBimodTRateProp0DiagDat.csv')
writetable(rItab,'CTBimodTRateProp0RDat.csv')

%% Part 7: Discrete time, debug network (geometrically distributed to have the same mean as before).

% We do not do proportions here, since the network is always random. This
% one is an 'ideal' scenario and thus we need minimal comparisons.

% Fixed transmission.
[infout,diagout,rIout] = discr0debugcompsimrep(0,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugFixedTRateProp1TSDat.csv')
writetable(diagtab,'DebugFixedTRateProp1DiagDat.csv')
writetable(rItab,'DebugFixedTRateProp1RDat.csv')

% Uniform distr.
[infout,diagout,rIout] = discr0debugcompsimrep(1,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugUnifTRateProp1TSDat.csv')
writetable(diagtab,'DebugUnifTRateProp1DiagDat.csv')
writetable(rItab,'DebugUnifTRateProp1RDat.csv')

% Bimodal distr.
[infout,diagout,rIout] = discr0debugcompsimrep(2,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugBimodTRateProp1TSDat.csv')
writetable(diagtab,'DebugBimodTRateProp1DiagDat.csv')
writetable(rItab,'DebugBimodTRateProp1RDat.csv')

%% Part 8: Continuous time version of the debug data (in case it is useful).

% A mirror of part 7 but in continuous time.

% Fixed transmission.
[infout,diagout,rIout] = contr0debugcompsimrep(0,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugCTFixedTRateProp1TSDat.csv')
writetable(diagtab,'DebugCTFixedTRateProp1DiagDat.csv')
writetable(rItab,'DebugCTFixedTRateProp1RDat.csv')

% Uniform distr.
[infout,diagout,rIout] = contr0debugcompsimrep(1,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugCTUnifTRateProp1TSDat.csv')
writetable(diagtab,'DebugCTUnifTRateProp1DiagDat.csv')
writetable(rItab,'DebugCTUnifTRateProp1RDat.csv')

% Bimodal distr.
[infout,diagout,rIout] = contr0debugcompsimrep(2,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugCTBimodTRateProp1TSDat.csv')
writetable(diagtab,'DebugCTBimodTRateProp1DiagDat.csv')
writetable(rItab,'DebugCTBimodTRateProp1RDat.csv')

%% Part 9: Completely homogeneous test set. Just fixed and uniform transmissions, for conciseness.
% Actually, psyche, we're doing the whole damn set.
% Discrete time only.
% Fixed transmission.
[infout,diagout,rIout] = discr0homogcompsimrep(0,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'HomogFixedTRateProp1TSDat.csv')
writetable(diagtab,'HomogFixedTRateProp1DiagDat.csv')
writetable(rItab,'HomogFixedTRateProp1RDat.csv')

% Uniform distr.
[infout,diagout,rIout] = discr0homogcompsimrep(1,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'HomogUnifTRateProp1TSDat.csv')
writetable(diagtab,'HomogUnifTRateProp1DiagDat.csv')
writetable(rItab,'HomogUnifTRateProp1RDat.csv')

% Bimod dist.

[infout,diagout,rIout] = discr0homogcompsimrep(2,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'HomogBimodTRateProp1TSDat.csv')
writetable(diagtab,'HomogBimodTRateProp1DiagDat.csv')
writetable(rItab,'HomogBimodTRateProp1RDat.csv')

%% Part 10: The same again, but with asymmetric transmission, in case that matters...
% Discrete time only.
% Fixed transmission.
[infout,diagout,rIout] = discr0homogcompsimrep(0,40,0,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'HomogFixedTRateProp1TSDat.csv')
writetable(diagtab,'HomogFixedTRateProp1DiagDat.csv')
writetable(rItab,'HomogFixedTRateProp1RDat.csv')

% Uniform distr.
[infout,diagout,rIout] = discr0homogcompsimrep(1,40,0,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'HomogUnifTRateProp1TSDat.csv')
writetable(diagtab,'HomogUnifTRateProp1DiagDat.csv')
writetable(rItab,'HomogUnifTRateProp1RDat.csv')

%% Part 11: A last-ditch debug effort, where we strip out the weighted random uses.

% Fixed transmission.
[infout,diagout,rIout] = discr0unweighteddebugcompsimrep(0,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugFixedTRateProp1TSDat.csv')
writetable(diagtab,'DebugFixedTRateProp1DiagDat.csv')
writetable(rItab,'DebugFixedTRateProp1RDat.csv')

% Uniform distr.
[infout,diagout,rIout] = discr0unweighteddebugcompsimrep(1,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugUnifTRateProp1TSDat.csv')
writetable(diagtab,'DebugUnifTRateProp1DiagDat.csv')
writetable(rItab,'DebugUnifTRateProp1RDat.csv')

% Bimodal distr.
[infout,diagout,rIout] = discr0unweighteddebugcompsimrep(2,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugBimodTRateProp1TSDat.csv')
writetable(diagtab,'DebugBimodTRateProp1DiagDat.csv')
writetable(rItab,'DebugBimodTRateProp1RDat.csv')

%% Part 12: Bonus simulations examining if the R0 estimate is useful as a predictor of the epidemic threshold.

% The successful one.
[infout,diagout,rIout] = discr0debugthreshsimrep(3,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','Extinct','ExtTime','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugAbThreshFixedTRateProp1TSDat.csv')
writetable(diagtab,'DebugAbThreshFixedTRateProp1DiagDat.csv')
writetable(rItab,'DebugAbThreshFixedTRateProp1RDat.csv')

% The unsuccessful one.
[infout,diagout,rIout] = discr0debugthreshsimrep(4,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','Extinct','ExtTime','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'DebugBeThreshFixedTRateProp1TSDat.csv')
writetable(diagtab,'DebugBeThreshFixedTRateProp1DiagDat.csv')
writetable(rItab,'DebugBeThreshFixedTRateProp1RDat.csv')

%% Part 13: Further threshold evaluation, for the community structure simulations.
% Prop 1 first.

% The successful one.
[infout,diagout,rIout] = discr0commstructthreshsimrep(5,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','Extinct','ExtTime','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CSAbThreshFixedTRateProp1TSDat.csv')
writetable(diagtab,'CSAbThreshFixedTRateProp1DiagDat.csv')
writetable(rItab,'CSAbThreshFixedTRateProp1RDat.csv')

% The unsuccessful one.
[infout,diagout,rIout] = discr0commstructthreshsimrep(6,40,1,1,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','Extinct','ExtTime','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CSBeThreshFixedTRateProp1TSDat.csv')
writetable(diagtab,'CSBeThreshFixedTRateProp1DiagDat.csv')
writetable(rItab,'CSBeThreshFixedTRateProp1RDat.csv')

% Prop 0 next.

% The successful one.
[infout,diagout,rIout] = discr0commstructthreshsimrep(5,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','Extinct','ExtTime','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CSAbThreshFixedTRateProp0TSDat.csv')
writetable(diagtab,'CSAbThreshFixedTRateProp0DiagDat.csv')
writetable(rItab,'CSAbThreshFixedTRateProp0RDat.csv')

% The unsuccessful one.
[infout,diagout,rIout] = discr0commstructthreshsimrep(6,40,1,0,200);

inftab = array2table(infout);
diagtab = array2table(diagout,"VariableNames",{'Rep','SProp','Extinct','ExtTime','EEIProp','Delta','kh','kh2','kh3'});
rItab = array2table(rIout,"VariableNames",{'ID','nConn','SecInfs','Gen','Rep'});

writetable(inftab,'CSBeThreshFixedTRateProp0TSDat.csv')
writetable(diagtab,'CSBeThreshFixedTRateProp0DiagDat.csv')
writetable(rItab,'CSBeThreshFixedTRateProp0RDat.csv')