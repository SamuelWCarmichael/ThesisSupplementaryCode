% This version of the non-retroleptomonad model uses a gamma distribution
% of mean 6 and scale variable of 0.5 instead of an exponential
% distribution (for bite frequency only).

function F = simulateSandflyGammaNonRetro_PIM()
% No input currently, since this function shouldn't require any.

% Loading some important global variables.
global meanLS meanBiteFreq biteScaleParam lifespanReduction

lifespan = exprnd(meanLS); % Choosing a lifespan for the fly from an exponential random variable.

firstBiteT = gamrnd(meanBiteFreq,biteScaleParam); % Choose the time for the first bite, exponential dist. in this version.

firstNectoPeakT = firstBiteT + (2.8 + 0.4*rand); % Nectomonads peak after 3 days, a simplification made by the model.

% If this first peak will happen after the death of the fly, we might as
% well not run the simulation and give a zero vector output.

if firstNectoPeakT > lifespan
    F.popArray = [0,0,0]; % Population values over time.
    F.transVec = [0,0,0,0,0]; % Record of whether given bites were successful transmissions.
    F.t = [0]; % Time vector.
    F.numBites = 0; % Number of bites.
    F.numTrans = 0; % Number of successful transmissions.
    F.lifespan = lifespan;
    F.pTr = [0];
    return
end

% Assuming now that the first nectomonad peak will happen before the fly
% dies, we set up an event array (well, vector really). 

if firstBiteT <= 0.001
    evtA = " "+string(firstBiteT)+" Bite"+" NA"; % An edge case, the whitespace improves the sorter.
elseif firstBiteT >= 10 && int8(firstBiteT) == firstBiteT
    % The rare case of an integer >= 10 needs special handling to ensure it
    % is sorted into the correct place.
    if lifespan >= 100
        evtA = string(0)+string(firstBiteT)+"."+"0"+" Bite"+" NA";
    else
        evtA = string(firstBiteT)+"."+"0"+" Bite"+" NA";
    end
elseif firstBiteT < 10 && lifespan >= 100
    evtA = string(0)+string(0)+string(firstBiteT)+" Bite"+" NA"; % The 0 is for the benefit of the sorter.
elseif (firstBiteT < 10 && lifespan < 100) || (firstBiteT >= 10 && firstBiteT < 100 && lifespan >= 100)
    evtA = string(0)+string(firstBiteT)+" Bite"+" NA";
else
    evtA = string(firstBiteT)+" Bite"+" NA"; % This may look strange, but is a sortable string holding all event information.
end
evtArray = [evtA]; % The event strings are stored in this vector.

ICVec = [0;0;0]; % No nectomonads, leptomonads, or metacyclics to start.
pVecOut = []; % Empty output population vector.
tVecOut = []; % Empty output time vector.
evtCount = 1; % Counter for the event vector.
elapsedTime = 0.0; % Elapsed time - updated as events happens, helps drive the while loop below.
loopCount = 0; % Failsafe counter to ensure the while loop stops even if something goes wrong.
infState = "Naive"; % This is used to determine which model to run. Starts out naive, becomes infected potentially.
numBites = 0; % Counts the number of bites taken over the life of the sandfly.
numTrans = 0; % Number of successful transmissions.
transVec = [0,0,0,0,0]; % Indicates if bites 1-5+ transmitted. Bite 1 should never transmit!
pTr = []; % Holding vector for the number of metas transferred on each bite.

while elapsedTime < lifespan
    % This is the main while loop driving the simulation. It should always
    % stop, but I'm building in a failsafe too.
    loopCount = loopCount + 1;
    if loopCount >= 100
        "100 loops exceeded! Terminating!"
        break % 100 events is very unlikely.
    end
    curEvt = evtArray(evtCount); % Extract the current event.
    curEvt = strip(curEvt,'left'); % Stripping some additional whitespace added in certain contexts.
    curEvtSplit = split(curEvt); % Split it at the whitespace.
    endTime = double(curEvtSplit(1)); % Time is at the front...
    evtType = curEvtSplit(2); % ... event type at the back.
    numNecto = double(curEvtSplit(3)); % Also the number of nectomonads to add - sometimes this will be NA.
    % It is possible (if rare) for two events to coincide. This needs
    % handling overwise it causes linspace to crash my code!
    if elapsedTime ~= endTime
        % We now have the time we need to run ode45 for. Some ode45 prep:
        tSpan = linspace(elapsedTime, endTime, 100); % A suitable time vector.
    
        % We have all the components now. This was, unfortunately, the easy
        % bit. There are a few possible scenarios that could arise each cycle.
        % These must all be covered.
    
        % This model is easier - we always run ode45 with the same model.
        [t, pVec] = ode45(@noRetroModel, tSpan, ICVec);
    
        % We will also need to handle the output of that ode45 run in the same
        % way (initially) to ensure no duplicate times occur etc.
    
        elapsedTime = t(end); % Elapsed time is now the endTime from before.
        tVecOut = [tVecOut; t(1:end-1)]; % The last t value will be the first of the next run, we omit it for now.
        ICVec = pVec(end,:); % Next IC vector is the end pop values of the last run.
        pVecOut = [pVecOut; pVec(1:end-1,:)]; % Same treatment as for t.
    end
    
    % What happens next depends on two things: the state of the system, and
    % what type of event happened.
    
    if evtType == "Necto"
        % The event was a nectomonad peak. We add the number of nectomonads
        % onto the ICVec, update the state to "infected" if necessary, and
        % that's it.
        ICVec(1) = ICVec(1) + numNecto;
        if infState == "Naive"
            infState = "Infected"; % Updated infection status.
            % This is where we implement parasite-induced mortality. It is
            % crude, IE a fixed reduction independent of how infected a fly
            % is, and entirely educated guesswork.
            remLifespan = lifespan - elapsedTime; % Calculate remaining life...
            lostLifespan = lifespanReduction*remLifespan; % Lose some proportion of this...
            lifespan = lifespan - lostLifespan; % Adjust lifespan accordingly.
            % elapsedTime > lifespan is assured, but the while loop will
            % catch it if something goes wrong.
        end
    elseif evtType == "Bite" && infState == "Naive"
        % The event was a bite, but the fly is still naive. It will uptake
        % parasites (potentially) but not transfer. We also need to
        % calculate another bite time/ necto peak and add these events to
        % the vector.
        pTr = [pTr 0]; % Never any parasites transferred if not infected!
        numBites = numBites + 1;
        numAmast = biteCalculator(); % Calculate if/number of parasites uptaken.
        if numAmast > 0
            % If parasites are uptaken/infected host is bitten, otherwise
            % nothing will happpen after the bite.
            nextNectoPeak = 3.0*numAmast; % Multiply that by 3.
            nectoPeakT = elapsedTime + (2.8 + 0.4*rand); % Nectomonad peak happens 3 days after bite.
            if nectoPeakT < lifespan
                if nectoPeakT < 10 && lifespan >= 100
                    newNectoEvt = string(0)+string(0)+string(nectoPeakT)+" Necto "+string(nextNectoPeak);
                elseif nectoPeakT >= 10 && int8(nectoPeakT) == nectoPeakT
                    if lifespan >= 100
                        newNectoEvt = string(0)+string(nectoPeakT)+"."+"0"+" Necto "+string(nextNectoPeak);
                    else
                        newNectoEvt = string(nectoPeakT)+"."+"0"+" Necto "+string(nextNectoPeak);
                    end
                elseif (nectoPeakT < 10 && lifespan < 100) || (nectoPeakT >= 10 && nectoPeakT < 100 && lifespan >= 100)
                    newNectoEvt = string(0)+string(nectoPeakT)+" Necto "+string(nextNectoPeak);
                else
                    newNectoEvt = string(nectoPeakT)+" Necto "+string(nextNectoPeak); % Prepare the new event.
                end
                evtArray = [evtArray newNectoEvt]; % Add it to the array - will be sorted later.
            end
        end
        if numBites < 50
            nextBiteT = elapsedTime + gamrnd(meanBiteFreq, biteScaleParam); % Generating next bite time.
        else
            nextBiteT = lifespan + 1.0;
        end
        if nextBiteT < lifespan
            % If the fly will still be alive when the bite occurs:
            if nextBiteT <= 0.001
                newBiteEvt = " "+string(nextBiteT)+" Bite"+" NA";
            elseif nextBiteT >= 10 && int8(nextBiteT) == nextBiteT
                if lifespan >= 100
                    newBiteEvt = string(0)+string(nextBiteT)+"."+"0"+" Bite"+" NA";
                else
                    newBiteEvt = string(nextBiteT)+"."+"0"+" Bite"+" NA";
                end
            elseif nextBiteT < 10 && lifespan >= 100
                newBiteEvt = string(0)+string(0)+string(nextBiteT)+" Bite"+" NA";
            elseif (nextBiteT < 10 && lifespan < 100) || (nextBiteT >= 10 && nextBiteT < 100 && lifespan >= 100)
                newBiteEvt = string(0)+string(nextBiteT)+" Bite"+" NA";
            else
                newBiteEvt = string(nextBiteT)+" Bite"+" NA"; % Add it to the event vector, as before.
            end
            evtArray = [evtArray newBiteEvt]; 
        end
    elseif evtType == "Bite" && infState == "Infected"
        % Much like the last outcome, but this time the fly is infected and
        % can transfer parasites to the host.
        numBites = numBites + 1;
        numAmast = biteCalculator(); % Calculate if/number of parasites uptaken.
        if numAmast > 0
            % If parasites are uptaken/infected host is bitten, otherwise
            % nothing will happpen after the bite.
            nextNectoPeak = 3.0*numAmast; % Multiply that by 3.
            nectoPeakT = elapsedTime + (2.8 + 0.4*rand); % Nectomonad peak happens 3 days after bite.
            if nectoPeakT < lifespan
                if nectoPeakT < 10 && lifespan >= 100
                    newNectoEvt = string(0)+string(0)+string(nectoPeakT)+" Necto "+string(nextNectoPeak);
                elseif nectoPeakT >= 10 && int8(nectoPeakT) == nectoPeakT
                    if lifespan >= 100
                        newNectoEvt = string(0)+string(nectoPeakT)+"."+"0"+" Necto "+string(nextNectoPeak);
                    else
                        newNectoEvt = string(nectoPeakT)+"."+"0"+" Necto "+string(nextNectoPeak);
                    end
                elseif (nectoPeakT < 10 && lifespan < 100) || (nectoPeakT >= 10 && nectoPeakT < 100 && lifespan >= 100)
                    newNectoEvt = string(0)+string(nectoPeakT)+" Necto "+string(nextNectoPeak);
                else
                    newNectoEvt = string(nectoPeakT)+" Necto "+string(nextNectoPeak); % Prepare the new event.
                end
                evtArray = [evtArray newNectoEvt]; % Add it to the array - will be sorted later.
            end
        end
        if numBites < 50
            nextBiteT = elapsedTime + gamrnd(meanBiteFreq, biteScaleParam); % Generating next bite time.
        else
            nextBiteT = lifespan + 1.0;
        end
        if nextBiteT < lifespan
            % If the fly will still be alive when the bite occurs:
            if nextBiteT <= 0.001
                newBiteEvt = " "+string(nextBiteT)+" Bite"+" NA";
            elseif nextBiteT >= 10 && int8(nextBiteT) == nextBiteT
                if lifespan >= 100
                    newBiteEvt = string(0)+string(nextBiteT)+"."+"0"+" Bite"+" NA";
                else
                    newBiteEvt = string(nextBiteT)+"."+"0"+" Bite"+" NA";
                end
            elseif nextBiteT < 10 && lifespan >= 100
                newBiteEvt = string(0)+string(0)+string(nextBiteT)+" Bite"+" NA";
            elseif (nextBiteT < 10 && lifespan < 100) || (nextBiteT >= 10 && nextBiteT < 100 && lifespan >= 100)
                newBiteEvt = string(0)+string(nextBiteT)+" Bite"+" NA";
            else
                newBiteEvt = string(nextBiteT)+" Bite"+" NA"; % Add it to the event vector, as before.
            end
            evtArray = [evtArray newBiteEvt]; 
        end
        % Finally, we work out how many parasites are transferred to the
        % host (if any).
        biteOutcome = transmissionCalculator(ICVec); % A struct variable!
        pTr = [pTr biteOutcome.lostMetas];
        % Based on this output, we modify things.
        ICVec(2) = ICVec(2) - biteOutcome.lostLeptos;
        ICVec(3) = ICVec(3) - biteOutcome.lostMetas;
        if biteOutcome.transmission == 1
            if numBites < 5
                transVec(numBites) = 1;
            else
                transVec(5) = transVec(5) + 1;
            end
            numTrans = numTrans + 1;
        end
    elseif evtType == "Death"
        % Ensures we escape the loop.
        elapsedTime = elapsedTime + 1;
    end
    % Now we've either added a new event or ran out of lifespan. We cover
    % for the latter case by adding a "Death" event. We also sort the list.
    if evtCount == length(evtArray)
        % This means we're out of events - the final event is death.
        deathEvt = string(lifespan) + " Death" + " NA";
        evtArray = [evtArray deathEvt];
        evtCount = evtCount + 1; % Now we have one final event, we just added it.
    else
        % We have more events left.
        evtCount = evtCount + 1;
    end
    % Now, sort the event array (ensures correct event order).
    evtArray = sort(evtArray);
    
end

% By this point, we should have ran the fly for its full lifespan. I've
% been collecting 'data' as it went, we just need to arrange this in output
% variable F (another struct variable for versatility.

F.t = tVecOut;
F.popArray = pVecOut;
F.numBites = numBites;
F.numTrans = numTrans;
F.transVec = transVec;
F.lifespan = lifespan;
F.pTr = pTr;

end

%% This is a subfunction for calculating details regarding the bites taken by the sandfly.

function F = biteCalculator()

global infHostChance k paraBurd bloodMV

% First, we check if the fly is biting an infected host.
p = rand();
if p <= infHostChance
    % We've bitten an infected host and can uptake parasites.
    mu = paraBurd*bloodMV;
    var = mu*(1 + mu/k);
    % We're drawing the no. parasites from a negative binomial
    % distribution. Thus we need P (probability of successful result) and R
    % (dispersion factor).
    P = mu/var;
    R = (mu^2)/(var-mu);
    % Using nbinrnd for this:
    F = nbinrnd(R,P);
else
    % No infection, no parasite uptake.
    F = 0;
end

end

%% This subfunction calculates whether or not transmission occurs during a bite - and how many parasites are transferred.

function F = transmissionCalculator(x)

global transLow transHigh transThreshold loadThreshold

% 'x' is the current fly parasite values.
if x(2) + x(3) < loadThreshold
    % Low parasite load, transfer 0.15% of leptomonads and metacyclics.
    F.lostLeptos = x(2)*transLow;
    F.lostMetas = x(3)*transLow;
    if F.lostMetas >= transThreshold
        % Transmission occurs - but under default conditions, this never
        % happens at low parasite loads.
        F.transmission = 1; % Actually a logic operator.
    else
        F.transmission = 0;
    end
else
    % High parasite load, transfer 14% of leptomonads and metacyclics.
    F.lostLeptos = x(2)*transHigh;
    F.lostMetas = x(3)*transHigh;
    if F.lostMetas >= transThreshold
        % Transmission occurs - under default conditions, we need ~6800
        % metacyclics to do this.
        F.transmission = 1;
    else
        F.transmission = 0;
    end
end

end