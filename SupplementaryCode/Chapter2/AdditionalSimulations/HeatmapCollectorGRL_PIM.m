% This script generates data for my heatmap type graphs. It will take a
% long time to run, worth leaving it going by itself - I'm building in
% status updates so its progress can be monitored.

% First we run the parameters file.
run modelparams.m
% Readying the globals.
global k paraBurd
% Next we need to set up an output variable - a cell array of some
% predefined size, ideally.
head = {'NumTrans' 'Lifespan' 'k' 'MeanParasiteBurden'}; % Header row.
output = cell(3990000,4); % Might need to tweak the size later.
outputV2 = zeros(1995000,53);
% Nested loop time - not ideal but can't devise an alternative.
rowCount = 0; % My counter to ensure we remain in the correct row.
for i = 0.2:0.1:2.0
    for j = 10^3:4950:10^5
        % Some debug readouts:
        "We are now processing k ="
        i
        "and paraBurd ="
        j
        % i represents our k value, j represents our mean parasite load.
        % I intend, thus to set the global parameters to these values.
        k = i;
        paraBurd = j;
        % We need to store the number of transmission and lifespan for
        % each fly, additionally with k/paraBurd to enable sorting of the
        % data in RStudio.
        % I'm running 10000 flies, the more we do the longer it takes.
        for f = 1:5000
            rowCount = rowCount + 1;
            flyOut = simulateSandflyGammaRetro_PIM();
            numTrans = flyOut.numTrans;
            lifespan = flyOut.lifespan;
            pTr = flyOut.pTr;
            flyRow = { numTrans lifespan k paraBurd };
            flyRowV2 = [k, paraBurd, lifespan];
            flyRowV2 = [flyRowV2 pTr];
            outputV2(rowCount,1:length(flyRowV2)) = flyRowV2;
            output(rowCount, :) = flyRow;
            % We will collect more data here too. For now, I'm not sure
            % what else I will need.
        end
    end
end
% Need to store this in a csv for processing later.
outTab = cell2table(output,'VariableNames',head);
%writetable(outTab, 'GRLHeatmapDat.csv');
csvwrite('GRLHeatmapDat_PIM.csv',outputV2);