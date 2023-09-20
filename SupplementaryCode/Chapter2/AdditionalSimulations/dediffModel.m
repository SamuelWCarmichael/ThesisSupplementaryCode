% This script contains the dedifferentiation dynamics (conversion of
% metacyclics to retroleptomonads). This is suitable for use with ode45 and
% accepts time as a dummy variable. Uses parameters in modelparams.m.

function F = dediffModel(~,x)
global alpha r s u g q cc
% First we extract the population variables.
N = x(1); % Nectomonads
L = x(2); % Leptomonads
M = x(3); % Metacyclics
R = x(4); % Retroleptomonads

dN = - alpha*N; % Nectomonad replication.
dL = alpha*N + r*L*(1.0-((N+L+M+R)/cc)) - s*L; % Leptomonad replication - left in longer form for clarity.
dM = s*L - u*M - g*M; % Metacyclic replication - now with dedifferentiation!
dR = q*R*(1.0-((N+L+M+R)/cc)) + g*M; % Retroleptomonad replication.

F = [dN; dL; dM; dR];
end
