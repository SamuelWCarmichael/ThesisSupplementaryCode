% This is my random choice function for arbitrary unweighted lists, and
% needs a 1-by-n or n-by-1 array of ids.

function F = pickRandom(vec)

n = length(vec);
n2 = [1:n];
pv = 1 - n2/n;
p = rand();
ind = pv < p;
index = min(n2(ind));
val = vec(index);

F = val; % NOTE: Don''t forget to change this!
end