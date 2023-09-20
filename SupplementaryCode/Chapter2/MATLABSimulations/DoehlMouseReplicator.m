% This script is much like the Doehl replicator but it runs for 9
% representative mice and only generates 1000 flies.

run modelparams.m
% Readying the globals.
global k paraBurd biteChance
% Next we need to set up an output variable - a cell array of some
% predefined size, ideally.
head = {'NumParas' 'Lifespan' 'k' 'MeanParasiteBurden'}; % Header row.
%output = cell(3990000,4); % Might need to tweak the size later.
outputV2 = zeros(18000,4);
biteChance = 1.0; % This needs manually setting for fear of running out of RAM!
% Nested loop time - not ideal but can't devise an alternative.
rowCount = 0; % My counter to ensure we remain in the correct row.
%RAG 1
"Currently processing RAG 1"
k = 1.2697;
paraBurd = 10^3.2;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 1];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 2
"Currently processing RAG 2"
k = 0.9156;
paraBurd = 10^3.1;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 2];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 3
"Currently processing RAG 3"
k = 1.2474;
paraBurd = 10^3.2;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 3];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 4
"Currently processing RAG 4"
k = 3.2960;
paraBurd = 10^3.0;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 4];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 5
"Currently processing RAG 5"
k = 0.9523;
paraBurd = 10^3.0;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 5];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 6
"Currently processing RAG 6"
k = 0.7229;
paraBurd = 10^4.0;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 6];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 7
"Currently processing RAG 7"
k = 0.6561;
paraBurd = 10^5.0;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 7];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 8
"Currently processing RAG 8"
k = 0.8124;
paraBurd = 10^5.0;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 8];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end
% RAG 9
"Currently processing RAG 9"
k = 0.7222;
paraBurd = 10^4.0;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 9];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 10
"Currently processing RAG 10"
k = 0.9081;
paraBurd = 1.057*10^4;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 10];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 11
"Currently processing RAG 11"
k = 0.7723;
paraBurd = 6.2977*10^3;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 11];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 12
"Currently processing RAG 12"
k = 1.1523;
paraBurd = 9.424*10^3;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 12];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 13
"Currently processing RAG 13"
k = 1.0847;
paraBurd = 8.6053*10^6;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 13];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 14
"Currently processing RAG 14"
k = 1.2146;
paraBurd = 6.7356*10^4;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 14];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 15
"Currently processing RAG 15"
k = 0.5017;
paraBurd = 2.1384*10^4;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 15];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 16
"Currently processing RAG 16"
k = 1.2758;
paraBurd = 1.6032*10^4;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 16];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 17
"Currently processing RAG 17"
k = 1.2462;
paraBurd = 1.1973*10^5;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 17];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

%RAG 18
"Currently processing RAG 18"
k = 1.6440;
paraBurd = 9.315*10^4;
for f = 1:1000
    rowCount = rowCount + 1;
    flyOut = simulateSandflyDoehlTest();
    numParas = sum(flyOut.popArray(end,:));
    lifespan = flyOut.lifespan;
    flyRowV2 = [k, paraBurd, 18];
    flyRowV2 = [flyRowV2 numParas];
    outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
    %output(rowCount, :) = flyRow;
    % We will collect more data here too. For now, I'm not sure
    % what else I will need.
end

% This should be the end of our mice, but more can be added later.

% Need to store this in a csv for processing later.
%outTab = cell2table(output,'VariableNames',head);
%writetable(outTab, 'GNRHeatmapDat.csv');
csvwrite('DoehlMouseReplicator.csv',outputV2);