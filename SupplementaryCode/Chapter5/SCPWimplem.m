% This is an ode45-friendly implementation of the Corcoran and Hastings
% simplification of the SCPW model. This might be the closest to an
% 'exact' endemic equilibrium that we are likely to come.

function F = SCPWimplem(~,Q)

global kh alpha beta delta

v = Q(1);
w = Q(2);
x = Q(3);
y = Q(4);
z = Q(5);

dv = w - kh*delta*x;
dw = kh*delta*x - w;
dx = z - (delta + 1)*x + (alpha*delta*v*x*(y - x))/(kh*(x + y)^2) + (beta*delta*x*(y - x))/(x + y);
dy = 2*x - (2*alpha*delta*v*x*y)/(kh*(x + y)^2) - (2*beta*delta*x*y)/(x + y);
dz = -2*z + 2*delta*x + (2*alpha*delta*v*x^2)/(kh*(x + y)^2) + (2*beta*delta*x^2)/(x + y);

F = [dv;dw;dx;dy;dz];
end