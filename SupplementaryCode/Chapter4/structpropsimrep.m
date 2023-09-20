% This is a replicator script that allows you to set up a structured
% community, and then randomise a fixed proportion of connections. The
% randomised connections are re-assigned as per a config model (allowing
% self and duplicate edges, chosen by pairing stubs randomly).

function [F,G] = structpropsimrep(r,gam,reps,prop)
% This runs reps number of sims, with recovery rate gam and per-neighbour
% transmission rate r. Since we use discrete time, r/gam are both
% per-timestep chances. prop is the proportion of edges randomised.

endtime = 2000; % Fixed end time for all simulations.
indvs = 1200; % Number of individuals per simulation.
initInfs = 30; % Initial infections to seed in each sim. Elevated numbers this time.
nParas = 3; % Number of paras. Note that indvs really needs to divide nicely into nParas...

diagnostics = zeros(reps,9);
infoutput = zeros(reps,endtime);

for rep = 1:reps
    disp('Beginning Replication Number:')
    disp(rep)
    % For each simulation we want to record the total number of infected
    % individuals for each timestep, and some diagnostic data. We return
    % both as separate outputs so we can store them in different places.
    indvArray = zeros(indvs,4); % This will hold all information about all individuals, their id, para, household and infection status (and more later, maybe).
    infArray = []; % Records info about infected individuals, though may not end up used since we need to longterm archiving.
    conMat = []; % Records information about connections between individuals.
    activeInfs = []; % 'Ticking' infections, i.e. ones which are actively infectious.
    t = 0; % Might as well initialise global time here so it is not forgotten.
    
    % This time, the connection array generation will take part in two
    % different phases. First we set up the structured community, then we
    % erase some proportion of connections, and finally we randomly
    % reconnect the newly-vacant stubs.

    maxi = 0; % We use this to give us the id for each individual.
    hId = 0;
    for para = 1:nParas
        tInd = para*indvs/nParas; % The maximum index we want to generate.
        while maxi < tInd
            % Using while loops can be risky, so we must be careful.
            if tInd - maxi <= 6
                % We have enough people for a household, and I want to
                % avoid creating 1-man houses.
                hsize = tInd - maxi;
            else
                % A not-quite-uniform distribution between 
                hsize = round(4*rand()) + 2;
            end
            hId = hId + 1;
            for ind = 1:hsize
                indent = [ind+maxi,para,hId,0];
                indvArray(ind+maxi,:) = indent;
            end
            % Now we connect all the individuals up. Later they will have
            % external connections too, but for now we want internal only.
            for i1 = 1:hsize-1
                for i2 = i1+1:hsize
                    conMatEnt = [i1+maxi,i2+maxi,r;i2+maxi,i1+maxi,r];
                    conMat = [conMat;conMatEnt];
                end
            end
            maxi = maxi + hsize;
        end
    end

    % Next we pair up the houses... sort of. For the sake of maximal
    % structure, we make a connection between each household and the
    % household with the next id. We also have a 5% chance of introducing
    % an inter-para connection.

    maxH = max(indvArray(:,3)); % Max household id number.
    hhVec = 1:maxH;

    for hh = 1:maxH
        iids = indvArray(indvArray(:,3)==hh,1);
        cid1 = pickRandom(iids);
        if hh == maxH
            nbhID = 1;
        else
            nbhID = hh + 1;
        end
        nbiids = indvArray(indvArray(:,3)==nbhID,1);
        cid2 = pickRandom(nbiids);
        conMatEnt = [cid1,cid2,r;cid2,cid1,r];
        conMat = [conMat;conMatEnt];
        % Next we run the 5% inter-para check.
        if rand() <= 0.05 && ~isempty(AinB(hh,hhVec)) % This largely is "if hh is still in hhVec, i.e. not already paired.
            cid1 = pickRandom(iids);
            cPara = indvArray(cid1,2); % Heh, shortcut to find the current para. Nearly missed this...
            hhList = indvArray(indvArray(:,2)~=cPara,3);
            cHHs = AinB(hhVec,hhList); % Custom function, effectively removes duplicate entries from hhList.
            nbhID = pickRandom(cHHs);
            nbiids = indvArray(indvArray(:,3)==nbhID,1);
            cid2 = pickRandom(nbiids);
            conMatEnt = [cid1,cid2,r;cid2,cid1,r];
            conMat = [conMat;conMatEnt];
            % Actually, I don't really want duplicates, so we'll remove the
            % two houses from hhVec.
            hhVec = hhVec(hhVec~=hh);
            hhVec = hhVec(hhVec~=nbhID);
        end
    end

    indvec = 1:indvs;

    % The second phase: We have our (arguably) over-structured connection
    % array. Now we cleave some proportion of connections.
    if prop > 0
        nEdges = length(conMat(:,1))/2; % Half because we store edges in identical pairs (i.e. A-B and B-A both present).
        nRem = round(prop*nEdges);
        nVec = 1:nEdges;
        nVecRef = nVec;
        for n = 1:nRem
            % We pick an edge each time, and remove it from nVec.
            eid = pickRandom(nVec);
            nVec = nVec(nVec~=eid);
        end
        rEVec0 = AnotinB(nVecRef,nVec);
        eVec1 = 2*nVec;
        eVec2 = 2*nVec - 1;
        rEVec1 = 2*rEVec0;
        rEVec2 = 2*rEVec0 - 1;
        eVec = [eVec1,eVec2];
        rEVec = [rEVec1,rEVec2];
        nConn = zeros(indvs,1); % We populate this soon.
        for i = rEVec
            id = conMat(i,1);
            nConn(id) = nConn(id) + 1;
        end
        conMat = conMat(eVec,:);
    
        % Now... we attempt to pair the remaining stubs. This will use the
        % config model approach.
        
        wghArray = [indvec;nConn'];
        ids = indvec; % A simple duplicate, one which we modify.
        wghArray = wghArray(:,wghArray(2,:) > 0); % Update wghArray by removing all individuals with no remaining stubs.
    
        while max(wghArray(2,:)) > 0
            % We repeat this until all connections are made.
            % Originally, this was completely random, but that causes some
            % problems in certain cases, so now we always make a connection for
            % the largest (or a random largest) degree individual.
            ind = pickWeightedRandom(wghArray); % We pick a stub uniformly at random, which means higher degree people more likely to be chosen.
            wghArray(2,wghArray(1,:)==ind) = wghArray(2,wghArray(1,:)==ind) - 1; % We will pick a second weighted random to pair to, but we must remove this stub first.
            nbh = pickWeightedRandom(wghArray); % Choosing a neighbour by the same method, a stub at random.
            pTIC = r; % In other contexts, this will be variable, not a fixed r.
            % EXPERIMENTAL: We won't add self/duplicate edges, but still count
            % them towards the nConn entry for each node.
            if ind ~= nbh
                % If ind is not equal to nbh, it is not a self edge.
                if isempty(conMat)
                    % No entries logged, so we can always add the edge if it is
                    % non-self.
                    conMatEnt = [ind,nbh,pTIC;nbh,ind,pTIC]; % Preparing an entry in the connection matrix.
                    conMat = [conMat;conMatEnt]; % Appending it, much to MATLABs dislike.
                else
                    if any(conMat(:,1)==ind)
                        % If ind is already in conMat, it already has at least one
                        % edge connected to it.
                        nbs = conMat(conMat(:,1)==ind,2); % Find all of ind's neighbours.
                        if ~any(nbs==nbh)
                            % If nbh is NOT in the list of neighbours, we can add the
                            % edge to the array.
                            conMatEnt = [ind,nbh,pTIC;nbh,ind,pTIC]; % Preparing an entry in the connection matrix.
                            conMat = [conMat;conMatEnt]; % Appending it, much to MATLABs dislike.
                        end
                    else
                        % Otherwise, it has no neighbours to check.
                        conMatEnt = [ind,nbh,pTIC;nbh,ind,pTIC]; % Preparing an entry in the connection matrix.
                        conMat = [conMat;conMatEnt]; % Appending it, much to MATLABs dislike.
                    end
                end
            end
            wghArray(2,wghArray(1,:)==nbh) = wghArray(2,wghArray(1,:)==nbh) - 1; % Remove a stub from nbh
            wghArray = wghArray(:,wghArray(2,:) > 0); % Update wghArray by removing all individuals with no remaining stubs.
        end
    end
    % We want to create nConn, but probably best doing this retroactively
    % since I need a modified version earlier. We can pull the number of
    % connections out of conMat.
    for nd = 1:indvs
        conns = conMat(conMat(:,1)==nd,:);
        nC = length(conns(:,1));
        nConn(nd) = nC;
    end
    % Hopefully we now have a fully populated connection matrix.
    % Next we set up the initial infections.
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
        newInf = [inf, nConn(inf), 0, 0]; % Formatted as [id, degree, secondary infections caused, time spent infected].
        activeInfs = [activeInfs;newInf];
    end
    
    timeseries = zeros(1,endtime); % Needs to be empty, since I do not know the max time yet.

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
        activeInfs(:,4) = activeInfs(:,4) + 1; % We check for expired infs after checking if they infect, but we update the times now.
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
                        infEnt = [nb,nConn(nb),0,0];
                        newInfs = [newInfs;infEnt];
                        activeInfs(activeInfs(:,1)==inf,3) = activeInfs(activeInfs(:,1)==inf,3) + 1;
                    end
                end
            end
        end
        % We've now iterated through each individual and calculated any new
        % infections, and updated activeInfs accordingly (and indvArray).
        % Now we move any 'recovered' people to infArray.
        ainfIDs = activeInfs(:,1);
        for ent = 1:length(ainfIDs)
            id = ainfIDs(ent);
            if rand() <= gam
                % If our infection time is equal to our chosen duration.
                oldInf = activeInfs(activeInfs(:,1)==id,:); % We save the entry.
                %infArray = [infArray;oldInf]; % We don't need infArray here.
                indvArray(id,4) = 0; % Reset the infection status to 0.
                activeInfs = activeInfs(activeInfs(:,1)~=id,:); % Remove it from activeInfs.
            end
        end
        activeInfs = [activeInfs;newInfs];
    end

    % We come to the end of a rep, and we calculate our diagnostic data
    % etc. and add it all to our output arrays.
    infoutput(rep,:) = timeseries;
    [iprop,deltf] = shortendeqpredictor(nConn,r,gam);
    diagnostics(rep,:) = [rep,prop,r,gam,iprop,deltf,mean(nConn),mean(nConn.^2),mean(nConn.^3)];
end

F = infoutput;
G = diagnostics;
end