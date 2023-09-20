% This is a discrete-time simulation of the initial stages of an outbreak
% of leishmaniasis. It is designed to accomoate a range of probability
% distributions for transmission rate r, a fixed duration of infections,
% and permanent immunity (unlike in Ch4). It tracks who each individual
% infects, as well as the generation that infected individual is part of.

function [F,G,H] = discr0debugcompsimrep(rType,infDur,symT, prop, reps)
% rType specifies which probability distribution to use.
% infDur is our fixed infectious duration. We could extend it to be
% variable with a little effort, but not needed yet.
% reps is the number of replications to run. Each sim will take time, but
% hopefully not too long.
% Prop allows us to randomise the network. It isn't going to be
% extensively used, but I might compare the really structured/unstructured
% sims.
% symT controls whether transmission is symmetric, i.e. is rij = rji.

endtime = 120; % Fixed end time for all simulations. Short, compared to ch4.
indvs = 800; % Number of individuals per simulation. Also smaller! Was 200.
initInfs = 20; % Initial infections to seed in each sim. Low numbers for now. Was 4.
nParas = 1; % Number of paras. A single para is all I want on this one.

diagnostics = zeros(reps,9);
infoutput = zeros(reps,endtime); %Probably won't need this, keeping just in case.
rIOut = []; % Impossible to predict the size of this one.

for rep = 1:reps
    disp('Beginning Replication Number:')
    disp(rep)
    % As output, we will need data for all infections: who did they
    % infected, what generation are they, etc.
    indvArray = zeros(indvs,4); % This will hold all information about all individuals, their id, para, household and infection status (and more later, maybe).
    infArray = []; % Records info about infected individuals, though may not end up used since we need to longterm archiving.
    conMat = []; % Records information about connections between individuals.
    activeInfs = []; % 'Ticking' infections, i.e. ones which are actively infectious.
    recInfs = []; % Recovered infections. It is mostly inert, individuals in here are harvested at the end though.
    t = 0; % Might as well initialise global time here so it is not forgotten.
    
    % This time we have a really simple setup where we just generate a
    % nConn from a suitable distribution and set up a fully random config
    % model with no houses or anything. This is an 'ideal' setup for the R0
    % estimates.
    indvec = 1:indvs;
    nConn = geornd(0.2577,1,indvs);
    nConn = nConn + 1; % A correction to ensure everyone has at least 1 edge.
    wghArray = [indvec;nConn];
    ids = indvec; % A simple duplicate, one which we modify.
    wghArray = wghArray(:,wghArray(2,:) > 0); % Update wghArray by removing all individuals with no remaining stubs. % Probably not needed?

    while max(wghArray(2,:)) > 0
        % We repeat this until all connections are made.
        % Originally, this was completely random, but that causes some
        % problems in certain cases, so now we always make a connection for
        % the largest (or a random largest) degree individual.
        ind = pickWeightedRandom(wghArray); % We pick a stub uniformly at random, which means higher degree people more likely to be chosen.
        wghArray(2,wghArray(1,:)==ind) = wghArray(2,wghArray(1,:)==ind) - 1; % We will pick a second weighted random to pair to, but we must remove this stub first.
        nbh = pickWeightedRandom(wghArray); % Choosing a neighbour by the same method, a stub at random.
        % EXPERIMENTAL: We won't add self/duplicate edges, but still count
        % them towards the nConn entry for each node.
        if ind ~= nbh
            % If ind is not equal to nbh, it is not a self edge.
            if isempty(conMat)
                % No entries logged, so we can always add the edge if it is
                % non-self.
                if symT == 1
                    r = newTRate(rType);
                    conMatEnt = [ind,nbh,r;nbh,ind,r]; % Preparing an entry in the connection matrix.
                else
                    conMatEnt = [ind,nbh,newTRate(rType);nbh,ind,newTRate(rType)]; % Preparing an entry in the connection matrix.
                end
                conMat = [conMat;conMatEnt]; % Appending it, much to MATLABs dislike.
            else
                if any(conMat(:,1)==ind)
                    % If ind is already in conMat, it already has at least one
                    % edge connected to it.
                    nbs = conMat(conMat(:,1)==ind,2); % Find all of ind's neighbours.
                    if ~any(nbs==nbh)
                        % If nbh is NOT in the list of neighbours, we can add the
                        % edge to the array.
                        if symT == 1
                            r = newTRate(rType);
                            conMatEnt = [ind,nbh,r;nbh,ind,r];
                        else
                            conMatEnt = [ind,nbh,newTRate(rType);nbh,ind,newTRate(rType)];
                        end
                        conMat = [conMat;conMatEnt]; % Appending it, much to MATLABs dislike.
                    end
                else
                    % Otherwise, it has no neighbours to check.
                    if symT == 1
                        r = newTRate(rType);
                        conMatEnt = [ind,nbh,r;nbh,ind,r]; % Preparing an entry in the connection matrix.
                    else
                        conMatEnt = [ind,nbh,newTRate(rType);nbh,ind,newTRate(rType)]; % Preparing an entry in the connection matrix.
                    end
                    conMat = [conMat;conMatEnt]; % Appending it, much to MATLABs dislike.
                end
            end
        end
        wghArray(2,wghArray(1,:)==nbh) = wghArray(2,wghArray(1,:)==nbh) - 1; % Remove a stub from nbh
        wghArray = wghArray(:,wghArray(2,:) > 0); % Update wghArray by removing all individuals with no remaining stubs.
    end
    % NOTE: I've found some odd behaviour that rarely triggers. Sometimes
    % we connect and individual to something with ID 0. Until I can divine
    % the source of this, I remove such entries.
    conMat = conMat(conMat(:,1)~=0,:);
    conMat = conMat(conMat(:,2)~=0,:);

    % We want to create nConn, but probably best doing this retroactively
    % since I need a modified version earlier. We can pull the number of
    % connections out of conMat.
    for nd = 1:indvs
        conns = conMat(conMat(:,1)==nd,:);
        nC = length(conns(:,1));
        nConn(nd) = nC;
    end
    % Seeding initial infections.
    uninfs = indvec;
    infs = zeros(1,initInfs);
    for nf = 1:initInfs
        % Each time we just pick a random individual to infect.
        newID = pickRandom(uninfs);
        %newID = pickWeightedRandom(uninfs2); % Either comment out this or the one before, don't use both!
        infs(nf) = newID;
        uninfs = uninfs(uninfs ~= newID);
    end
    indvArray(infs,4) = 1; % Log these in indvArray.
    % We now set up the activeInfs properly.
    for inf = infs
        % For each individual I create an entry and add it to activeInfs.
        % Currently all infections have same infDur. This script only does
        % constant infectious duration.
        newInf = [inf, nConn(inf), 0, 1, infDur]; % Formatted as [id, degree, secondary infections caused, generation, remaining infectious duration].
        activeInfs = [activeInfs;newInf];
    end
    timeseries = zeros(1,endtime);

    while t < endtime
        % Until the end time is reached, or we go extinct.
        if isempty(activeInfs) == 1
            %nExtinct = nExtinct + 1;
            disp('Infection is Extinct, Teminating...')
            break
        end
        % At the start of the timestep I want to capture the number of infected
        % individuals etc. for plotting later.
        % I also would be interesting in the number of susceptible individuals
        % accessible to the infection.
        infList = activeInfs(:,1); % A list of infected individuals.
        nInf = length(infList);
        nSus = 0; % Number of 'accessible' susceptibles.
        countedSus = [];
        for ctr = 1:nInf
            ii = infList(ctr);
            % For each infection, I want to gather all the susceptible
            % neighbours and count them up.
            nbList = [];
            nbList = conMat(conMat(:,1)==ii,2);
            for nbEnt = nbList'
                if indvArray(nbEnt,4) == 0 && ~any(countedSus == nbEnt)
                    nSus = nSus + 1;
                    countedSus = [countedSus,nbEnt];
                end
            end
            if length(countedSus) ~= nSus
                disp('Mismatch in nSus and countedSus!')
            end
        end
        nRec = 0;
        if ~isempty(infArray)
            nRec = length(infArray(:,1));
        end
        t = t + 1; % Global time tick.
        timeseriesent = [t;nInf;nSus;nRec]; % A relic from the original data handling. Not used currently but preserved in case of future use.
        timeseries(t) = nInf;
        activeInfs(:,5) = activeInfs(:,5) - 1; % We check for expired infs after checking if they infect, but we update the times now.
        % Usually, you can just use inf = activeInfs(:,1) directly. But it
        % wasn't working this time, so we use a more explicit, longer form.
        newInfs = [];
        for co = 1:length(activeInfs(:,1))
            inf = activeInfs(co,1);
            % For each active infection, we check whether we infect a
            % neighbour this timestep. This is a multistep process.
            nbs = conMat(conMat(:,1)==inf,2);
            ics = conMat(conMat(:,1)==inf,2:3);
            for nb = nbs'
                % For each neighbour we check their infection status.
                if indvArray(nb,4) == 0
                    % If they are uninfected we check to see if we infect.
                    p = rand();
                    thresh = ics(ics(:,1)==nb,2);
                    if p <= thresh
                        % We cause an infection.
                        indvArray(nb,4) = 1;
                        gen = activeInfs(activeInfs(:,1)==inf,4) + 1;
                        infEnt = [nb,nConn(nb),0,gen,infDur]; % Generation is always one higher than the infector.
                        newInfs = [newInfs;infEnt];
                        activeInfs(activeInfs(:,1)==inf,3) = activeInfs(activeInfs(:,1)==inf,3) + 1;
                    end
                end
            end
        end
        % We've now iterated through each individual and calculated any new
        % infections, and updated activeInfs accordingly (and indvArray).
        % Now we move any 'recovered' people to recInfs.
        ainfIDs = activeInfs(:,1);
        for ent = 1:length(ainfIDs)
            id = ainfIDs(ent);
            if activeInfs(activeInfs(:,1)==id,5) == 0
                % Remaining infectious duration is 0.
                oldInf = activeInfs(activeInfs(:,1)==id,:); % We save the entry.
                recInfs = [recInfs;oldInf]; % New row in recInfs. We will extract our data from recInfs at the end.
                activeInfs = activeInfs(activeInfs(:,1)~=id,:); % Remove it from activeInfs.
            end
        end
        activeInfs = [activeInfs;newInfs];
    end

    % We come to the end of a rep, and we calculate our diagnostic data
    % etc. and add it all to our output arrays.
    infoutput(rep,:) = timeseries;
    %[iprop,deltf] = shortendeqpredictor(nConn,r,gam);
    iprop = 0; % Placeholders. Probably just going to remove them.
    deltf = 0;
    gam = 0;
    r = 0; % Effectively rendered irrelevant. Really, most of the diag data is...
    diagnostics(rep,:) = [rep,prop,r,gam,iprop,deltf,mean(nConn),var(nConn),mean(nConn.^3)];
    recInfs(:,5) = rep; % Repurposing the old infection age column to be the rep now.
    rIOut = [rIOut;recInfs(:,1:5)];
end

F = infoutput;
G = diagnostics;
H = rIOut;
end