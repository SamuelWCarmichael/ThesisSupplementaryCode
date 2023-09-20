% This is a counterpart to AinB which does the opposite, selecting things
% that are not in B.

function F = AnotinB(A,B)

output = [];

for i = 1:length(A)
    ent = A(i);
    if ~any(B==ent)
        output = [output,ent];
    end
end

F = output;
end