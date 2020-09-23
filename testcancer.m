clear all

% Read cancer data
T = readtable('Colon.txt');
normData = table2array(T(:,startsWith(T.Properties.VariableNames,'normal')));
tumData = table2array(T(:,startsWith(T.Properties.VariableNames,'tumor')));


% Reduce normData
normDataTest = normData(:,end);
normData = normData(:,1 : end-1);


% Snapshots
[results,data] = snapshots(normData);


%%
% Eigenfunctions
M = results.eigenfunction;
M = M';

degree = []; % Dummy variable, not used in this code.
numofpoints = size(M,1);
data = linspace(0,1,numofpoints)';
n = size(M,2);
x = 0 : ( 1 / (numofpoints-1) ) : 1;

% Optional design matrix
polymodel.M = M;

% Number of columns in the design matrix
params.indexsetsize = n;


%%
% Create Multilevel Binary tree
fprintf('\n');
fprintf('Create KDd tree ---------------------------------\n');
tic;
[datatree, sortdata] = make_tree(data,@split_KD,params);
toc;

% Create multilevel basis
fprintf('\n');
fprintf('Create multilevel basis ------------------------\n');
tic;
[multileveltree, ind, datacell, datalevel]  = multilevelbasis(datatree, sortdata, degree, polymodel);
toc;


%% Plot coefficients
figure
%Q = normData(:,1);
%Q = normDataTest;

Q = tumData(:,1);
[coeff, levelcoeff, dcoeffs, ccoeffs] = hbtrans(Q, multileveltree, ind, datacell, datalevel);
maxlevel = max(levelcoeff);
numlevel = 6;
counter = 1;
subplot(numlevel + 2,1,counter);
plot(x,Q);
for n = maxlevel : -1 : maxlevel - numlevel
    counter = counter + 1;
    subplot(numlevel + 2, 1, counter);  
    stem(coeff(levelcoeff == n));
    title(['Level Coefficients = ',num2str(n)]);
end
