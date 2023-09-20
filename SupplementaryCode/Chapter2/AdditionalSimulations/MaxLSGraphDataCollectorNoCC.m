% This script is the first form of graph of transmission chance vs.
% lifespan.

% First we run the parameters file.
run modelparams.m
% Readying the globals.
global k paraBurd maxLifespan
% Next we need to set up an output variable - a cell array of some
% predefined size, ideally.
head = {'NumTrans' 'Lifespan' 'k' 'MeanParasiteBurden'}; % Header row.
%output = cell(3990000,4); % Might need to tweak the size later.
outputV2 = zeros(255000,54); % I hope this is the right size...
% Nested loop time - not ideal but can't devise an alternative.
rowCount = 0; % My counter to ensure we remain in the correct row.
% We want to repeat a loop for mouse 13 and a handful of others...

% RAG 1
k = 1.2697; 
paraBurd = 1*10^3.2;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 1;
    % Some debug readouts:
    "We are now processing max lifespan:"
    i
    
    "for RAG mouse:"
    mouse
    % We need to store the number of transmission and lifespan for
    % each fly, additionally with k/paraBurd to enable sorting of the
    % data in RStudio.
    % I'm running 5000 flies, the more we do the longer it takes.
    for f = 1:5000
        rowCount = rowCount + 1;
        flyOut = simulateSandflyVarMaxLSNoCC();
        numTrans = flyOut.numTrans;
        lifespan = flyOut.lifespan;
        numBites = flyOut.numBites;
        pTr = flyOut.pTr;
        %flyRow = { numTrans lifespan k paraBurd };
        flyRowV2 = [maxLifespan, numBites, lifespan, mouse];
        flyRowV2 = [flyRowV2 pTr];
        outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
        %output(rowCount, :) = flyRow;
        % We will collect more data here too. For now, I'm not sure
        % what else I will need.
    end
end

csvwrite('MaxLSVarNoCCDatM1.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;


% RAG 4
k = 3.2960;
paraBurd = 1*10^3.0; 
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 4;
    % Some debug readouts:
    "We are now processing max lifespan:"
    i
    
    "for RAG mouse:"
    mouse
    % We need to store the number of transmission and lifespan for
    % each fly, additionally with k/paraBurd to enable sorting of the
    % data in RStudio.
    % I'm running 5000 flies, the more we do the longer it takes.
    for f = 1:5000
        rowCount = rowCount + 1;
        flyOut = simulateSandflyVarMaxLSNoCC();
        numTrans = flyOut.numTrans;
        lifespan = flyOut.lifespan;
        numBites = flyOut.numBites;
        pTr = flyOut.pTr;
        %flyRow = { numTrans lifespan k paraBurd };
        flyRowV2 = [maxLifespan, numBites, lifespan, mouse];
        flyRowV2 = [flyRowV2 pTr];
        outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
        %output(rowCount, :) = flyRow;
        % We will collect more data here too. For now, I'm not sure
        % what else I will need.
    end
end

csvwrite('MaxLSVarNoCCDatM4.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 7
k = 0.6561; 
paraBurd = 1*10^5.0;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 7;
    % Some debug readouts:
    "We are now processing max lifespan:"
    i
    
    "for RAG mouse:"
    mouse
    % We need to store the number of transmission and lifespan for
    % each fly, additionally with k/paraBurd to enable sorting of the
    % data in RStudio.
    % I'm running 5000 flies, the more we do the longer it takes.
    for f = 1:5000
        rowCount = rowCount + 1;
        flyOut = simulateSandflyVarMaxLSNoCC();
        numTrans = flyOut.numTrans;
        lifespan = flyOut.lifespan;
        numBites = flyOut.numBites;
        pTr = flyOut.pTr;
        %flyRow = { numTrans lifespan k paraBurd };
        flyRowV2 = [maxLifespan, numBites, lifespan, mouse];
        flyRowV2 = [flyRowV2 pTr];
        outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
        %output(rowCount, :) = flyRow;
        % We will collect more data here too. For now, I'm not sure
        % what else I will need.
    end
end

csvwrite('MaxLSVarNoCCDatM7.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 13
k = 1.0847; 
paraBurd = 8.6053*10^6;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 13;
    % Some debug readouts:
    "We are now processing max lifespan:"
    i
    
    "for RAG mouse:"
    mouse
    % We need to store the number of transmission and lifespan for
    % each fly, additionally with k/paraBurd to enable sorting of the
    % data in RStudio.
    % I'm running 5000 flies, the more we do the longer it takes.
    for f = 1:5000
        rowCount = rowCount + 1;
        flyOut = simulateSandflyVarMaxLSNoCC();
        numTrans = flyOut.numTrans;
        lifespan = flyOut.lifespan;
        numBites = flyOut.numBites;
        pTr = flyOut.pTr;
        %flyRow = { numTrans lifespan k paraBurd };
        flyRowV2 = [maxLifespan, numBites, lifespan, mouse];
        flyRowV2 = [flyRowV2 pTr];
        outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
        %output(rowCount, :) = flyRow;
        % We will collect more data here too. For now, I'm not sure
        % what else I will need.
    end
end

csvwrite('MaxLSVarNoCCDatM13.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 18
k = 1.6440; 
paraBurd = 9.315*10^4;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 18;
    % Some debug readouts:
    "We are now processing max lifespan:"
    i
    
    "for RAG mouse:"
    mouse
    % We need to store the number of transmission and lifespan for
    % each fly, additionally with k/paraBurd to enable sorting of the
    % data in RStudio.
    % I'm running 5000 flies, the more we do the longer it takes.
    for f = 1:5000
        rowCount = rowCount + 1;
        flyOut = simulateSandflyVarMaxLSNoCC();
        numTrans = flyOut.numTrans;
        lifespan = flyOut.lifespan;
        numBites = flyOut.numBites;
        pTr = flyOut.pTr;
        %flyRow = { numTrans lifespan k paraBurd };
        flyRowV2 = [maxLifespan, numBites, lifespan, mouse];
        flyRowV2 = [flyRowV2 pTr];
        outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
        %output(rowCount, :) = flyRow;
        % We will collect more data here too. For now, I'm not sure
        % what else I will need.
    end
end

% Need to store this in a csv for processing later.
%outTab = cell2table(output,'VariableNames',head);
%writetable(outTab, 'GRLHeatmapDat.csv');
csvwrite('MaxLSVarNoCCDatM18.csv',outputV2);