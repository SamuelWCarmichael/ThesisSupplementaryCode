% This generates a nice new transmission value for a given bond. These will
% be placeholder values initially, so check they are set correctly!

function F = newTRate(type)
% Each type is a different probability distribution. We use this function
% to control the specifics of the dist. and pass the type to it from main.
    if type == 0
        % Fixed transmission, i.e. no probability distribution.
        %F = 0.03; % Higher rate.
        F = 0.015; % Lower Rate.
        %F = 0.8; % Extreme debug rate.
    elseif type == 1
        % Uniform-rv transmission.
        %F = 0.02*rand()+0.02; % Higher rate.
        F = 0.01*rand()+0.01; % Lower rate.
    elseif type == 2
        % Strictly bimodal transmission rates.
%         if rand()<=0.02 % Higher rate.
%             F = 0.9;
% 
%         else
%             F = 0.0122;
%         end
        if rand()<=0.01 % Lower rate.
            F = 0.9;
        else
            F = 0.0061;
        end
    elseif type == 3
        % This is a special type. It tests whether the R0 value has a role
        % as a predictor of the epidemic threshold. This is the "above threshold" option. 
        % Only for the debug network!
        %F = 0.0054; % NeAb
        %F = 0.006; % Ab
        %F = 0.0066; % FaAb
        F = 0.008; % VFaAb
    elseif type == 4
        % This is the "below threshold" type, to go with 3.
        %F = 0.0042; % NeBe
        %F = 0.0036; % Be
        %F = 0.003; % FaBe
        F = 0.0015; % VFaBe
    elseif type == 5
        % This is a special type. It tests whether the R0 value has a role
        % as a predictor of the epidemic threshold. This is the "above threshold" option. 
        % Only for the comm. struct. network!
        %F = 0.0044; % NeAb
        %F = 0.005; % Ab
        %F = 0.0056; % FaAb
        F = 0.007; % VFaAb
    elseif type == 6
        % This is the "below threshold" type, to go with 5.
        %F = 0.0032; % NeBe
        %F = 0.0026; % Be
        %F = 0.002; % FaBe
        F = 0.0005; % VFaBe
    else
        % Invalid types should still assign a value, just in case. The
        % value doesn't matter because it should never happen.
        F = 0.2; % Probably reasonable? But I won't really need it.
    end
end