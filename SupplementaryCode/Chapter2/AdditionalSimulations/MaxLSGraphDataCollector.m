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
% We want to repeat a loop for each of the first 18 RAG mice. This will
% take some time.

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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM1.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 2
k = 0.9156;
paraBurd = 1*10^3.1;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 2;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM2.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 3
k = 1.2474;
paraBurd = 1*10^3.2;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 3;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM3.csv',outputV2);
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM4.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 5
k = 0.9523; 
paraBurd = 1*10^3.0;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 5;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM5.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 6
k = 0.7229; 
paraBurd = 1*10^4.0;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 6;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM6.csv',outputV2);
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM7.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 8
k = 0.8124; 
paraBurd = 1*10^5.0;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 8;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM8.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 9
k = 0.7222; 
paraBurd = 1*10^4.0;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 9;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM9.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 10
k = 0.9081; 
paraBurd = 1.057*10^4;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 10;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM10.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 11
k = 0.7723; 
paraBurd = 6.2977*10^3;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 11;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM11.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 12
k = 1.1523; 
paraBurd = 9.424*10^3;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 12;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM12.csv',outputV2);
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM13.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 14
k = 1.2146; 
paraBurd = 6.7356*10^4;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 14;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM14.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 15
k = 0.5017; 
paraBurd = 2.1384*10^4;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 15;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM15.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 16
k = 1.2758; 
paraBurd = 1.6032*10^4;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 16;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM16.csv',outputV2);
outputV2 = zeros(255000,54);
rowCount = 0;

% RAG 17
k = 1.2462; 
paraBurd = 1.1973*10^5;
for i = 5:0.5:30
    maxLifespan = i;
    mouse = 17;
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
        flyOut = simulateSandflyVarMaxLS();
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

csvwrite('MaxLSVarDatM17.csv',outputV2);
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
        flyOut = simulateSandflyVarMaxLS();
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
csvwrite('MaxLSVarDatM18.csv',outputV2);