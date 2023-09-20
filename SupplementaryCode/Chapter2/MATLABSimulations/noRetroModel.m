% This script contains the original model (no retroleptomonads). It is
% useful for verifying the behaviour of the non-retroleptomonad system.
% This is intended for use with ode45 (accepts time as a dummy variable).

function F = noRetroModel(~,x)
global alpha r s u cc
% First we extract the population variables.
N = x(1); % Nectomonads
L = x(2); % Leptomonads
M = x(3); % Metacyclics

dN = - alpha*N; % Nectomonad replication.
dL = alpha*N + r*L*(1.0-((N+L+M)/cc)) - s*L; % Leptomonad replication - left in longer form for clarity.
dM = s*L - u*M; % Metacyclic replication.

F = [dN; dL; dM];
end
