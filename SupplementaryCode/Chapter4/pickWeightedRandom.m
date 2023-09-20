% This is my random choice function for weighted lists, requiring a
% two-by-n (or n-by-two) array, one row/column for ids and one for weights.

function F = pickWeightedRandom(arr)
% I want the array to be in 2-by-n form, so we correct if needed.
if length(arr(1,:)) == 2
    % If the first row is 2 in length, we transpose the array.
    arr = arr';
end
% Now, the first row is the ids, the second is the weights.
ids = arr(1,:);
wgh = arr(2,:);
n = [1:length(ids)]; % Used to choose the correct id later, in case the ids are not in ascending order.
% We use cumsum to set up a vector of probabilities, of sorts. Then we draw
% a random number and ask which bin it lies within.
pv = 1 - cumsum(wgh)/sum(wgh);
p = rand();
ind = pv < p;
index = min(n(ind));
val = ids(index);

F = val;
end