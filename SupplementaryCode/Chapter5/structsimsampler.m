% This is a really simple script that extracts sample networks from my
% structured sims, ones with a small number of nodes tyhat igraph can plot
% clearly enough.

indvs = 60; % Number of individuals per simulation.
initInfs = 30; % Initial infections to seed in each sim. Elevated numbers this time.
nParas = 1; % Number of paras. Note that indvs really needs to divide nicely into nParas...
prop = 0.5; % Proportion of edges randomised.
r = 0.05;

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

A = adjmatExporter(conMat,'midpropstrsample.csv');