% A simple function I call repeatedly in different places. It adds an entry
% to a sorted array and maintains the order. We assume the entry is vector
% of n length, and the matrix and nxm matrix, and we take the final vector
% entry to be the sorting value (usually time).

function F = addSortArray(v,A)

% NOTE: We assume v to be 1xn, a row. A has n columns and m rows.
% The method here is to compile a new array using 3 bits.
if isempty(A)
    F = v; % Unless it is empty, in which case we just set it up.
else
    less = A(A(:,end)<=v(end),:); % For ties (which are possible) we handle them first-come first-served.
    more = A(A(:,end)>v(end),:);
    F = [less;v;more];
end
end