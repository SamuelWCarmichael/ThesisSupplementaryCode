% This script contains the new retroleptomonad model
% (post-dedifferentiation) and is intended for use with ode45 (time
% accepted as a dummy variable).

function F = retroModelSinks(~,x)
global alpha r s u q v cc lossRate
% First we extract the population variables.
N = x(1); % Nectomonads
L = x(2); % Leptomonads
M = x(3); % Metacyclics
R = x(4); % Retroleptomonads

dN = - alpha*N - lossRate*N; % Nectomonad replication.
dL = alpha*N + r*L*(1.0-((N+L+M+R)/cc)) - s*L - lossRate*L; % Leptomonad replication - left in longer form for clarity.
dM = s*L - u*M + v*R - lossRate*M; % Metacyclic replication.
dR = q*R*(1.0-((N+L+M+R)/cc)) - v*R - lossRate*R; % Retroleptomonad replication.

F = [dN; dL; dM; dR];
end
