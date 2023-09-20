% A continuous version of the discrete replicator.

function [F,G,H] = contr0debugcompsimrep(rType,infDur,symT, prop, reps)
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
    nConn = geornd(0.2049,1,indvs);
    wghArray = [indvec;nConn];
    ids = indvec; % A simple duplicate, one which we modify.
    wghArray = wghArray(:,wghArray(2,:) > 0); % Update wghArray by removing all individuals with no remaining stubs. % Probably not needed?

    % The second phase: We have our (arguably) over-structured connection
    % array. Now we cleave some proportion of connections.
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

    % New stuff for the Next Reaction Method (the Gibson-Bruck algorithm,
    % it is sometimes called). Events go in a queue, and time follows the
    % events. No need to iterate in discrete steps. We set up the initial
    % events now. For fixed infectious duration, we can always add one
    % event per case. We also cheat: if an infection won't occur in time,
    % we don't add it to the array.
    eventArr = [];
    for i = 1:length(activeInfs(:,1))
        id = activeInfs(i,1);
        nbs = conMat(conMat(:,1)==id,2);
        for j = 1:length(nbs)
            nb = nbs(j);
            if indvArray(nb,4) == 0
                % If nb is susceptible still, we attempt to generate a
                % transmission event.
                conSub = conMat(conMat(:,1)==id,:);
                tRate = conSub(conSub(:,2)==nb,3);
                eTime = t + exprnd(1/tRate);
                if eTime < infDur
                    % If the event happens before recovery.
                    evt = [1,id,nb,eTime];
                    eventArr = addSortArray(evt,eventArr);
                end
            end
        end
        % We also add a recovery event for all individuals that are
        % infected, using the fixed infDur. This will need adapting if
        % variable recovery times are ever implemented.
        evt = [2,id,0,infDur];
        eventArr = addSortArray(evt,eventArr);
    end

    while t < endtime
        % Until the end time is reached, or we go extinct.
        if isempty(activeInfs) == 1
            %nExtinct = nExtinct + 1;
            disp('Infection is Extinct, Terminating...')
            break
        end
        % To minimise disruption due to my edits, I will leave activeInfs
        % in place. We will handle changes by event though, so we will
        % mostly not need to check it very often.
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
        % Now, we do not need to use a fixed step. We pull the time from
        % the event matrix.
        nextEvt = eventArr(1,:); % We pull the next event in the chain out.
        t = nextEvt(4); % Event time is the new global time (which may be the same as before, if multiple events co-occur).
        eventArr = eventArr(2:end,:);
        % The next section is an overhauled version of the previous form of
        % this. Now changes to infections occur as per the events, and we
        % devote our effort to maintaining the event array.
        if nextEvt(1) == 1
            % Event type 1 is where we infect a neighbour.
            inf = nextEvt(2);
            nb = nextEvt(3);
            % First, the easy bit: infecting nb. Now, we don't actually
            % know if nb can be infected, so we check that first.
            if indvArray(nb,4) == 0
                % if it can be infected, we do stuff. Otherwise nothing
                % happens, since it will have been done already.
                indvArray(nb,4) = 1;
                gen = activeInfs(activeInfs(:,1)==inf,4) + 1;
                activeInfs(activeInfs(:,1)==inf,3) = activeInfs(activeInfs(:,1)==inf,3) + 1;
                infEnt = [nb,nConn(nb),0,gen,infDur]; % Generation is always one higher than the infector.
                activeInfs = [activeInfs;infEnt]; % We add it directly to the array, rather than waiting until the end of the phase.
                % Now, the infection is ready, and we calculate all associated
                % events. A little more sophisticated.
                recEvt = [2,nb,0,t+infDur];
                eventArr = addSortArray(recEvt,eventArr); % We always need one of these.
                nbs = conMat(conMat(:,1)==nb,2);
                for j = 1:length(nbs)
                    nb2 = nbs(j); % nb2 is a neighbour of nb, which is a neighbour of id.
                    if indvArray(nb2,4) == 0
                        conSub = conMat(conMat(:,1)==nb,:);
                        tRate = conSub(conSub(:,2)==nb2,3);
                        eTime = t + exprnd(1/tRate);
                        if eTime < t+infDur
                            % If the event happens before recovery.
                            evt = [1,nb,nb2,eTime];
                            eventArr = addSortArray(evt,eventArr);
                        end
                    end
                end
            end
        elseif nextEvt(1) == 2
            % Event type 2 is recovery, nice and simple.
            recID = nextEvt(2);
            oldInf = activeInfs(activeInfs(:,1)==recID,:); % We save the entry.
            recInfs = [recInfs;oldInf]; % New row in recInfs. We will extract our data from recInfs at the end.
            activeInfs = activeInfs(activeInfs(:,1)~=recID,:); % Remove it from activeInfs.
        else
            % There really aren't other event types, so this just displays
            % an error.
            disp('Invalid Event Type!')
            return % Currently ending things if this happens, it really shouldn't be and could be quite important.
        end
    end

    % We come to the end of a rep, and we calculate our diagnostic data
    % etc. and add it all to our output arrays.
    infoutput(rep,:) = timeseries;
    %[iprop,deltf] = shortendeqpredictor(nConn,r,gam);
    iprop = 0; % Placeholders. Probably just going to remove them.
    deltf = 0;
    gam = 0;
    r = 0;
    diagnostics(rep,:) = [rep,prop,r,gam,iprop,deltf,mean(nConn),mean(nConn.^2),mean(nConn.^3)];
    recInfs(:,5) = rep; % Repurposing the old infection age column to be the rep now.
    rIOut = [rIOut;recInfs(:,1:5)];
end

F = infoutput;
G = diagnostics;
H = rIOut;
end