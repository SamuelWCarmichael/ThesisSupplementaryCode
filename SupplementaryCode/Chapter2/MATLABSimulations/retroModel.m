% This script contains the new retroleptomonad model
% (post-dedifferentiation) and is intended for use with ode45 (time
% accepted as a dummy variable).

function F = retroModel(~,x)
global alpha r s u q v cc
% First we extract the population variables.
N = x(1); % Nectomonads
L = x(2); % Leptomonads
M = x(3); % Metacyclics
R = x(4); % Retroleptomonads

dN = - alpha*N; % Nectomonad replication.
dL = alpha*N + r*L*(1.0-((N+L+M+R)/cc)) - s*L; % Leptomonad replication - left in longer form for clarity.
dM = s*L - u*M + v*R; % Metacyclic replication.
dR = q*R*(1.0-((N+L+M+R)/cc)) - v*R; % Retroleptomonad replication.

F = [dN; dL; dM; dR];
end
