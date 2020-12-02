# Julio Change Detection
# testkarhunenloevelmultileveltransform.m
# 20201107 Xiaoyang Chen

`%notin%` <- Negate(`%in%`)
###Data and parameters
degree <-matrix(NaN,1,1)
numofpoints <-500

d <-1
data <-as.matrix(t(seq(0,d,len=numofpoints)))
n <-10
x <-as.matrix(seq(0,d,len=numofpoints))
M <-matrix(1,dim(x)[1],dim(x)[2])
Lc <-0.01
Lp <-max(d,2*Lc)
L  <-Lc / Lp
i=1
eigenlambda <-matrix(sqrt(sqrt(pi)*L / 2),1,n)

for (i in 2 : n)
  {
  if (floor(i/2) == i/2 ) 
  phi=(1 / i) * sin ( floor(i/2) * pi * x / Lp ) else
  phi=(1 / i) * cos ( floor(i/2) * pi * x / Lp )
  
  eigenlambda[1,i]=sqrt(sqrt(pi) * L) * exp( - ( (floor(i/2) * pi * L)^2 ) / 8 )
  M <-cbind(M, phi)
  }

eigenlambda = t(eigenlambda) 
polymodel<-data.frame(M)
indexsetsize = n
params<-data.frame(indexsetsize)

### Create Multilevel Binary tree
setwd("H:/2021Sep/Julio/ChangeDetection/ChangeDetectionR")
source("make_tree.R")
a<-make_tree(data,split_KD,params)

###  Create multilevel basis
source("multilevelbasis.R")
multilevelbasis

###  Run transform with random data-Orginal realization of the stochastic process
cte<-as.matrix(sqrt(3) * 2 * (runif(n) - 1/2))
Q = 1 + M %*%(cte*eigenlambda)
plot(x,Q,title(main='KL Realization'))

### Metrics
numvecs  <- ncol(Q)
maxerror=Inf
maxwaverror=Inf

#tic; set timer to R
for (n in 1:numvecs){
data.frame(coeff, levelcoeff, dcoeffs, ccoeffs) <- hbtrans(Q[,n], multileveltree, ind, datacell, datalevel)
Qv  <- invhbtrans(dcoeffs, ccoeffs, multileveltree, ind, datacell, datalevel, numofpoints)
polyerror(n)  <- norm(Q[,n] - Qv, 2) / norm(Q[,n])
wavecoeffnorm(n)  <- norm(coeff[1 : end - params.indexsetsize],'inf')/norm(Q[,n])
}
#t  <- toc

#Test format change from vector of coefficients to struct
totalerror=0
as.matrix(a,b) <- hbvectortocoeffs(coeff, multileveltree, ind, datacell, datalevel, numofpoints)
for (i in 1 : length(dcoeffs)){
totalerror <- totalerror + (norm[a[i] -  dcoeffs[i]])
end
totalerror <- totalerror + (norm[b -  ccoeffs])
print('Total error = %e \n', totalerror)
}

### Plot coefficients
figure(2)
maxlevel = max(levelcoeff)
numlevel = 3
counter = 1
subplot(numlevel + 2,1,counter)
plot(x,Q)
for (n in maxlevel : -1 : maxlevel - numlevel){
counter = counter + 1
subplot(numlevel + 2, 1, counter, title('Level Coefficients = ',num2str(n))) #TBD
stem(coeff(levelcoeff == n))
}

### Run trans
print('\n');
print('Add Bump to KL  --------------------------------\n')

#Random realization
#Q = polymodel.M * rand(size(polymodel.M,2),1);

#Add Gaussian "bump" to the data
sbump = 0.5
Qkl = Q
sigma = 0.001
maxbump = 0.05
bump <- maxbump * exp(- ((t(x) - sbump)^2) /sigma)
bump[1:150] = 0
bump[350:end] = 0
Q = Q + bump

numvecs = ncol(Q)
maxerror =Inf
maxwaverror =Inf
#tic;
for (n in 1 : numvecs){
    data.frame(coeff, levelcoeff, dcoeffs, ccoeffs) = hbtrans(Q[,n], multileveltree, ind, datacell, datalevel)
    Qv = invhbtrans(dcoeffs, ccoeffs, multileveltree, ind, datacell, datalevel, numofpoints)
    polyerror[n] = norm(Q[,n] - Qv, 2) / norm(Q[,n])
    wavecoeffnorm[n] = norm(coeff[1 : end - params.indexsetsize],'inf')/norm(Q[,n])
}
#t = toc;

