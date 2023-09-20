% A quick script to generate sample networks (small ones!) to output as an
% adjacency matrix and plot in RStudio. Uses my adjmatExporter script for
% conciseness. Currently does 4 networks, more can be added later.

%% First the bog standard homogeneous network.

indvs = 60;
symT = 1;
rType = 1; % Not relevant, needed for code.
nParas = 1; % Floof's sake, so is this.

indvArray = zeros(indvs,4); % This will hold all information about all individuals, their id, para, household and infection status (and more later, maybe).
infArray = []; % Records info about infected individuals, though may not end up used since we need to longterm archiving.
conMat = [];

indvec = 1:indvs;
nConn = zeros(1,indvs);
nConn(1,:) = 5;
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

adjmat1 = adjmatExporter(conMat,'homogsampleadjmat.csv');

%% Next, the geometric distribution network.

indvArray = zeros(indvs,4); % This will hold all information about all individuals, their id, para, household and infection status (and more later, maybe).
infArray = []; % Records info about infected individuals, though may not end up used since we need to longterm archiving.
conMat = [];

indvec = 1:indvs;
nConn = geornd(0.2577,1,indvs);
nConn = nConn + 1;
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

adjmat2 = adjmatExporter(conMat,'geometsampleadjmat.csv');

%% Next, the fully structured community structure sims.

prop = 0;
indvArray = zeros(indvs,4); % This will hold all information about all individuals, their id, para, household and infection status (and more later, maybe).
infArray = []; % Records info about infected individuals, though may not end up used since we need to longterm archiving.
conMat = [];

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
            indent = [ind+maxi,para,hId,0]; % Currently ID, para, household, infection status.
            indvArray(ind+maxi,:) = indent;
        end
        % Now we connect all the individuals up. Later they will have
        % external connections too, but for now we want internal only.
        for i1 = 1:hsize-1
            for i2 = i1+1:hsize
                if symT == 1
                    r = newTRate(rType);
                    conMatEnt = [i1+maxi,i2+maxi,r;i2+maxi,i1+maxi,r];
                else
                    conMatEnt = [i1+maxi,i2+maxi,newTRate(rType);i2+maxi,i1+maxi,newTRate(rType)];
                end
                conMat = [conMat;conMatEnt];
            end
        end
        maxi = maxi + hsize;
    end
end

% Next we pair up the houses... sort of. For the sake of maximal
% structure, we make a connection between each household and the
% household with the next id.

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
    if symT == 1
        r = newTRate(rType);
        conMatEnt = [cid1,cid2,r;cid2,cid1,r];
    else
        conMatEnt = [cid1,cid2,newTRate(rType);cid2,cid1,newTRate(rType)];
    end
    conMat = [conMat;conMatEnt];
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
end
% NOTE: I've found some odd behaviour that rarely triggers. Sometimes
% we connect and individual to something with ID 0. Until I can divine
% the source of this, I remove such entries.
conMat = conMat(conMat(:,1)~=0,:);
conMat = conMat(conMat(:,2)~=0,:);

adjmat3 = adjmatExporter(conMat,'commstructp0sampleadjmat.csv');

%% Finally, the fully random comm. struct. sims.

prop = 1;
indvArray = zeros(indvs,4); % This will hold all information about all individuals, their id, para, household and infection status (and more later, maybe).
infArray = []; % Records info about infected individuals, though may not end up used since we need to longterm archiving.
conMat = [];

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
            indent = [ind+maxi,para,hId,0]; % Currently ID, para, household, infection status.
            indvArray(ind+maxi,:) = indent;
        end
        % Now we connect all the individuals up. Later they will have
        % external connections too, but for now we want internal only.
        for i1 = 1:hsize-1
            for i2 = i1+1:hsize
                if symT == 1
                    r = newTRate(rType);
                    conMatEnt = [i1+maxi,i2+maxi,r;i2+maxi,i1+maxi,r];
                else
                    conMatEnt = [i1+maxi,i2+maxi,newTRate(rType);i2+maxi,i1+maxi,newTRate(rType)];
                end
                conMat = [conMat;conMatEnt];
            end
        end
        maxi = maxi + hsize;
    end
end

% Next we pair up the houses... sort of. For the sake of maximal
% structure, we make a connection between each household and the
% household with the next id.

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
    if symT == 1
        r = newTRate(rType);
        conMatEnt = [cid1,cid2,r;cid2,cid1,r];
    else
        conMatEnt = [cid1,cid2,newTRate(rType);cid2,cid1,newTRate(rType)];
    end
    conMat = [conMat;conMatEnt];
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
end
% NOTE: I've found some odd behaviour that rarely triggers. Sometimes
% we connect and individual to something with ID 0. Until I can divine
% the source of this, I remove such entries.
conMat = conMat(conMat(:,1)~=0,:);
conMat = conMat(conMat(:,2)~=0,:);

adjmat4 = adjmatExporter(conMat,'commstructp1sampleadjmat.csv');