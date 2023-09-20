% This is a version of my multipart rep script that focuses on the
% structured comunity with different levels of structured-ness.

%% Fully Structured (Prop = 0)

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.0;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp0TSOutput.csv')
        writetable(diagout,'StructProp0Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp0TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp0Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 1.0 (Fully Random)

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 2.0;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp1TSOutput.csv')
        writetable(diagout,'StructProp1Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp1TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp1Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.5

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.5;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp05TSOutput.csv')
        writetable(diagout,'StructProp05Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp05TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp05Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.1

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.1;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp01TSOutput.csv')
        writetable(diagout,'StructProp01Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp01TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp01Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.2

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.2;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp02TSOutput.csv')
        writetable(diagout,'StructProp02Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp02TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp02Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.3

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.3;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp03TSOutput.csv')
        writetable(diagout,'StructProp03Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp03TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp03Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.4

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.4;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp04TSOutput.csv')
        writetable(diagout,'StructProp04Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp04TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp04Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end


%% Prop = 0.6

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.6;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp06TSOutput.csv')
        writetable(diagout,'StructProp06Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp06TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp06Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.7

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.7;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp07TSOutput.csv')
        writetable(diagout,'StructProp07Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp07TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp07Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.8

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.8;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp08TSOutput.csv')
        writetable(diagout,'StructProp08Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp08TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp08Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

%% Prop = 0.9

gam = 0.025; % Recovery rate.
reps = 50; % Number of replicates per value of r.
prop = 0.9;
minr = 0.008; % Minimum infection rate.
maxr = 0.015; % Maximum infection rate.
i = 0; % Internal counter, related to csv writing tasks.

for r = minr:0.0007:maxr
    disp('Processing r = ')
    disp(r)
    [inf,diag] = structpropsimrep(r,gam,reps,prop);
    infout = array2table(inf);
    diagout = array2table(diag,"VariableNames",{'Rep','SProp','R','Gamma','EEIProp','Delta','kh','kh2','kh3'});
    if i == 0
        % If i = 0 we haven't yet created the csv files.
        writetable(infout,'StructProp09TSOutput.csv')
        writetable(diagout,'StructProp09Diagnostics.csv')
    else
        % We need to append instead of just writing, otherwise we lose the
        % previous set of data.
        writetable(infout,'StructProp09TSOutput.csv','WriteMode','append')
        writetable(diagout,'StructProp09Diagnostics.csv','WriteMode','append')
    end
    i = i+1;
end

