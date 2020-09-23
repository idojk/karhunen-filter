%Construct eigenfunction by using eigenvectors
function [results,data] = snapshots(data);
cx = data;
mx = size(cx,2);

% Compute eigenvectors and eigenvalues of covariance matrice

k = 21; %number of eigenvalues
M = size(cx,2);


% Run SVD to compute eigenvalues and eigenvectors

C = (cx' * cx) / M; %Covariance Matrix
[u,s,v] = svd(C);

s = diag(s);
s = s(1 : k);
u = u(:,1 : k);

results.covariance = C;
results.eigenvalues = s;
results.eigenvectors = u;

%Normalize eigenfunction
xefun = results.eigenvectors' * cx' ./sqrt(s)/sqrt(M);

results.eigenfunction = xefun;

end

%% Example
%load('example','x');
%snapshots(x);
