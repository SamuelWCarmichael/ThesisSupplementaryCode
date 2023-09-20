% This is a quick post-hoc script that prepares and exports an adjacency
% matrix for a network. We provide it the conMat and a filename, and it creates a binary
% indicator matrix.

function F = adjmatExporter(conMat,filename)

% conMat must be an array, filename must be a string, otherwise this will
% fail to work properly. % conMat should be formatted such that each row is
% a connection.

maxID = max(conMat(:,1));

adjmat = zeros(maxID);

for i = 1:length(conMat(:,1))
    id1 = conMat(i,1);
    id2 = conMat(i,2);
    adjmat(id1,id2) = 1;
end

writematrix(adjmat,filename);

F = adjmat; % Returning the adjacency matrix anyway, even though I'm writing it to a csv file.
end