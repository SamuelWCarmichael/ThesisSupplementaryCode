% This is a really basic function that I'm sure should exist in MATLAB but
% doesn't appear to. It takes 2 vectors, and extracts all entries in the
% first that are also in the second.

function F = AinB(A,B)

output = [];

for i = 1:length(A)
    ent = A(i);
    if any(B==ent)
        output = [output,ent];
    end
end

F = output;
end