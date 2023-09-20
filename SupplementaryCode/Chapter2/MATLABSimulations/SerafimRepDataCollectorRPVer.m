% This version of my serafim rep data collector also randomises the
% parameters slightly for each fly (+/- 10% by default).
run modelparams.m
headV2 = {'Lifespan' 'NumBites' 'NumTrans' 'B1T' 'B2T' 'B3T' 'B4T' 'B5T' 'Day2Metas' 'Day2MetaProp' ...
    'Day2Leptos' 'Day2LeptoProp' 'Day3Metas' 'Day3MetaProp' 'Day3Leptos' 'Day3LeptoProp' ...
    'Day6Metas' 'Day6MetaProp' 'Day6Leptos' 'Day6LeptoProp' 'Day9Metas' 'Day9MetaProp' ...
    'Day9Leptos' 'Day9LeptoProp' 'Day12Metas' 'Day12MetaProp' 'Day12Leptos' 'Day12LeptoProp' ...
    'Day13Metas' 'Day13MetaProp' 'Day13Leptos' 'Day13LeptoProp' 'Day14Metas' 'Day14MetaProp' ...
    'Day14Leptos' 'Day14LeptoProp' 'Day15Metas' 'Day15MetaProp' 'Day15Leptos' 'Day15LeptoProp' ...
    'Day18Metas' 'Day18MetaProp' 'Day18Leptos' 'Day18LeptoProp'};

% Some modifications I use for a side figure.
%global g v q
%g = 3.0;
%v = 3.0;
%q = 3.5;

SROut = cell(20000,44);
for i = 1:10000 
    outVec = cell(1,44); % Empty cell array (vector of sorts) of length 44.
    % Our first action is to simulate a sandfly.
    run modelparams.m % Resets the globals before the next run.
    paramRandomiser(); % Randomises the parameters again.
    flyout = simulateSandflySerafimRep();
    
    % This fly may or may not have actually bitten anyone.
    if flyout.numBites == 0
        % The fly died before biting anyone.
        outVec = {flyout.lifespan,0,0,0,0,0,0,0,0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA"};
        SROut(i,:) = outVec; % Add it to row i of the output matrix.
        % This is a boring outcome - we don't need any further processing.
    else
        % The fly has at least bitten someone - it may not have infected
        % anyone, but we need a bit more processing.
        outVec(1) = {flyout.lifespan};
        outVec(2) = {flyout.numBites};
        outVec(3) = {flyout.numTrans};
        outVec(4) = {flyout.transVec(1)};
        outVec(5) = {flyout.transVec(2)};
        outVec(6) = {flyout.transVec(3)};
        outVec(7) = {flyout.transVec(4)};
        outVec(8) = {flyout.transVec(5)};
        % That was the simple to extract data. Next part needs more
        % handling - we want to number/prop. of metacyclics are specific
        % days, which are unlikely to be present in t. Instead, we must
        % choose the most appropriate value.
        endTime = flyout.t(end); % This is useful for later - avoids bugs.
        % Serafim started at t = 2, so we shall do the same. We do not
        % expect any parasites to be detected yet, though - the model
        % doesn't allow a nectomonad peak before day 2.8!
        if endTime >= 2
            % We're going to choose the first t >= 3.
            j = 1; % A humble counter.
            while flyout.t(j) < 2
                j = j + 1; % Count up until t(j) has exceeded 3.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(9) = {pVec(3)};
            outVec(11) = {pVec(4)};
            if sum(pVec) == 0
                outVec(10) = {"NA"};
                outVec(12) = {"NA"};
            else
                outVec(10) = {pVec(3)/sum(pVec)};
                outVec(12) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 3
            % We're going to choose the first t >= 3.
            j = 1; % A humble counter.
            while flyout.t(j) < 3
                j = j + 1; % Count up until t(j) has exceeded 3.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(13) = {pVec(3)};
            outVec(15) = {pVec(4)};
            if sum(pVec) == 0
                outVec(14) = {"NA"};
                outVec(16) = {"NA"};
            else
                outVec(14) = {pVec(3)/sum(pVec)};
                outVec(16) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 6
            % We're going to choose the first t >= 6.
            j = 1; % A humble counter.
            while flyout.t(j) < 6
                j = j + 1; % Count up until t(j) has exceeded 6.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(17) = {pVec(3)};
            outVec(19) = {pVec(4)};
            if sum(pVec) == 0
                outVec(18) = {"NA"};
                outVec(20) = {"NA"};
            else
                outVec(18) = {pVec(3)/sum(pVec)};
                outVec(20) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 9
            % We're going to choose the first t >= 9.
            j = 1; % A humble counter.
            while flyout.t(j) < 9
                j = j + 1; % Count up until t(j) has exceeded 9.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(21) = {pVec(3)};
            outVec(23) = {pVec(4)};
            if sum(pVec) == 0
                outVec(22) = {"NA"};
                outVec(24) = {"NA"};
            else
                outVec(22) = {pVec(3)/sum(pVec)};
                outVec(24) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 12
            % We're going to choose the first t >= 12.
            j = 1; % A humble counter.
            while flyout.t(j) < 12
                j = j + 1; % Count up until t(j) has exceeded 12.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(25) = {pVec(3)};
            outVec(27) = {pVec(4)};
            if sum(pVec) == 0
                outVec(26) = {"NA"};
                outVec(28) = {"NA"};
            else
                outVec(26) = {pVec(3)/sum(pVec)};
                outVec(28) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 13
            % We're going to choose the first t >= 13.
            j = 1; % A humble counter.
            while flyout.t(j) < 13
                j = j + 1; % Count up until t(j) has exceeded 15.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(29) = {pVec(3)};
            outVec(31) = {pVec(4)};
            if sum(pVec) == 0
                outVec(30) = {"NA"};
                outVec(32) = {"NA"};
            else
                outVec(30) = {pVec(3)/sum(pVec)};
                outVec(32) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 14
            % We're going to choose the first t >= 14.
            j = 1; % A humble counter.
            while flyout.t(j) < 14
                j = j + 1; % Count up until t(j) has exceeded 18.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(33) = {pVec(3)};
            outVec(35) = {pVec(4)};
            if sum(pVec) == 0
                outVec(34) = {"NA"};
                outVec(36) = {"NA"};
            else
                outVec(34) = {pVec(3)/sum(pVec)};
                outVec(36) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 15
            % We're going to choose the first t >= 15.
            j = 1; % A humble counter.
            while flyout.t(j) < 15
                j = j + 1; % Count up until t(j) has exceeded 18.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(37) = {pVec(3)};
            outVec(39) = {pVec(4)};
            if sum(pVec) == 0
                outVec(38) = {"NA"};
                outVec(40) = {"NA"};
            else
                outVec(38) = {pVec(3)/sum(pVec)};
                outVec(40) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 18
            % We're going to choose the first t >= 18.
            j = 1; % A humble counter.
            while flyout.t(j) < 18
                j = j + 1; % Count up until t(j) has exceeded 18.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(41) = {pVec(3)};
            outVec(43) = {pVec(4)};
            if sum(pVec) == 0
                outVec(42) = {"NA"};
                outVec(44) = {"NA"};
            else
                outVec(42) = {pVec(3)/sum(pVec)};
                outVec(44) = {pVec(4)/sum(pVec)};
            end
        end
        SROut(i,:) = outVec;
    end    
end

for i = 10001:20000 
    outVec = cell(1,44); % Empty cell array (vector of sorts) of length 32.
    % Our first action is to simulate a sandfly.
    run modelparams.m % Resets the globals before the next run.
    paramRandomiser(); % Randomises the parameters again.
    flyout = simulateSandflySerafimRepOB();
    
    % This fly may or may not have actually bitten anyone.
    if flyout.numBites == 0
        % The fly died before biting anyone.
        outVec = {flyout.lifespan,0,0,0,0,0,0,0,0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA",0,"NA"};
        SROut(i,:) = outVec; % Add it to row i of the output matrix.
        % This is a boring outcome - we don't need any further processing.
    else
        % The fly has at least bitten someone - it may not have infected
        % anyone, but we need a bit more processing.
        outVec(1) = {flyout.lifespan};
        outVec(2) = {flyout.numBites};
        outVec(3) = {flyout.numTrans};
        outVec(4) = {flyout.transVec(1)};
        outVec(5) = {flyout.transVec(2)};
        outVec(6) = {flyout.transVec(3)};
        outVec(7) = {flyout.transVec(4)};
        outVec(8) = {flyout.transVec(5)};
        % That was the simple to extract data. Next part needs more
        % handling - we want to number/prop. of metacyclics are specific
        % days, which are unlikely to be present in t. Instead, we must
        % choose the most appropriate value.
        endTime = flyout.t(end); % This is useful for later - avoids bugs.
        % The first one, in fact, is going to be in t - it is the initial
        % value and serves as a debugging flag (if it isn't 0, something is
        % wrong!).
        outVec(9) = {flyout.popArray(1,3)};
        outVec(11) = {flyout.popArray(1,4)};
        if sum(flyout.popArray(1,:)) == 0
            outVec(10) = {"NA"};
            outVec(12) = {"NA"};
        else
            outVec(10) = {flyout.popArray(1,3)/sum(flyout.popArray(1,:))};
            outVec(12) = {flyout.popArray(1,4)/sum(flyout.popArray(1,:))};
        end
        % Now the fun starts, assuming lifespan >= 3.
        if endTime >= 3
            % We're going to choose the first t >= 3.
            j = 1; % A humble counter.
            while flyout.t(j) < 3
                j = j + 1; % Count up until t(j) has exceeded 3.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(13) = {pVec(3)};
            outVec(15) = {pVec(4)};
            if sum(pVec) == 0
                outVec(14) = {"NA"};
                outVec(16) = {"NA"};
            else
                outVec(14) = {pVec(3)/sum(pVec)};
                outVec(16) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 6
            % We're going to choose the first t >= 6.
            j = 1; % A humble counter.
            while flyout.t(j) < 6
                j = j + 1; % Count up until t(j) has exceeded 6.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(17) = {pVec(3)};
            outVec(19) = {pVec(4)};
            if sum(pVec) == 0
                outVec(18) = {"NA"};
                outVec(20) = {"NA"};
            else
                outVec(18) = {pVec(3)/sum(pVec)};
                outVec(20) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 9
            % We're going to choose the first t >= 9.
            j = 1; % A humble counter.
            while flyout.t(j) < 9
                j = j + 1; % Count up until t(j) has exceeded 9.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(21) = {pVec(3)};
            outVec(23) = {pVec(4)};
            if sum(pVec) == 0
                outVec(22) = {"NA"};
                outVec(24) = {"NA"};
            else
                outVec(22) = {pVec(3)/sum(pVec)};
                outVec(24) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 12
            % We're going to choose the first t >= 12.
            j = 1; % A humble counter.
            while flyout.t(j) < 12
                j = j + 1; % Count up until t(j) has exceeded 12.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(25) = {pVec(3)};
            outVec(27) = {pVec(4)};
            if sum(pVec) == 0
                outVec(26) = {"NA"};
                outVec(28) = {"NA"};
            else
                outVec(26) = {pVec(3)/sum(pVec)};
                outVec(28) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 13
            % We're going to choose the first t >= 13.
            j = 1; % A humble counter.
            while flyout.t(j) < 13
                j = j + 1; % Count up until t(j) has exceeded 15.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(29) = {pVec(3)};
            outVec(31) = {pVec(4)};
            if sum(pVec) == 0
                outVec(30) = {"NA"};
                outVec(32) = {"NA"};
            else
                outVec(30) = {pVec(3)/sum(pVec)};
                outVec(32) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 14
            % We're going to choose the first t >= 14.
            j = 1; % A humble counter.
            while flyout.t(j) < 14
                j = j + 1; % Count up until t(j) has exceeded 18.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(33) = {pVec(3)};
            outVec(35) = {pVec(4)};
            if sum(pVec) == 0
                outVec(34) = {"NA"};
                outVec(36) = {"NA"};
            else
                outVec(34) = {pVec(3)/sum(pVec)};
                outVec(36) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 15
            % We're going to choose the first t >= 15.
            j = 1; % A humble counter.
            while flyout.t(j) < 15
                j = j + 1; % Count up until t(j) has exceeded 18.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(37) = {pVec(3)};
            outVec(39) = {pVec(4)};
            if sum(pVec) == 0
                outVec(38) = {"NA"};
                outVec(40) = {"NA"};
            else
                outVec(38) = {pVec(3)/sum(pVec)};
                outVec(40) = {pVec(4)/sum(pVec)};
            end
        end
        if endTime >= 18
            % We're going to choose the first t >= 18.
            j = 1; % A humble counter.
            while flyout.t(j) < 18
                j = j + 1; % Count up until t(j) has exceeded 18.
            end
            % Now we extract the popVec row corresponding to j.
            pVec = flyout.popArray(j,:);
            outVec(41) = {pVec(3)};
            outVec(43) = {pVec(4)};
            if sum(pVec) == 0
                outVec(42) = {"NA"};
                outVec(44) = {"NA"};
            else
                outVec(42) = {pVec(3)/sum(pVec)};
                outVec(44) = {pVec(4)/sum(pVec)};
            end
        end
        SROut(i,:) = outVec;
    end    
end
outTab = cell2table(SROut,'VariableNames',headV2);
writetable(outTab, 'SROut.csv');

%% Functions, for parameter randomisation.
function paramRandomiser()
% This randomises our parameters a little. It's run a lot!
global alpha r s u g q v
alpha = 0.9*alpha + (0.2*alpha)*rand();
r = 0.9*r + (0.2*r)*rand();
s = 0.9*s + (0.2*s)*rand();
u = 0.9*u + (0.2*u)*rand();
g = 0.9*g + (0.2*g)*rand();
q = 0.9*q + (0.2*q)*rand();
v = 0.9*v + (0.2*v)*rand();
end