# Julio Change Detection
# testkarhunenloevelmultileveltransform.m
# 20201107 Xiaoyang Chen


`%notin%` <- Negate(`%in%`)

setwd("H:/2021Sep/Julio/ChangeDetection/ChangeDetectionR")
source("make_tree.R") #comment it when debugging
source("multilevelbasis.R")
source("hbtrans.R")
source("invhbtrans.R")

###Data and parameters
degree <-matrix(NaN,1,1)
numofpoints <-500

d <-1
data <-as.matrix(seq(0,d,len=numofpoints))
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

# Optional design matrix
polymodel<-list()
polymodel$M<-M
#class(polymodel$M)

indexsetsize = n
params<-data.frame(indexsetsize)



### Create Multilevel Binary tree
# start_time<- Sys.time()
mt<-make_tree(data,split_KD,params)
# end_time<- Sys.time()
# end_time-start_time

# check results
# str(mt)
# level5<-mt$tree$left$left$left$left$left
# mt[['tree']][['left']][['left']][['left']][['left']][['left']][['idxs']]
# mt[['tree']][['left']][['left']][['left']][['left']][['right']][['idxs']]
# right should be 17:32


###  Create multilevel basis
# start_time<- Sys.time()
mb<-multilevelbasis(mt$tree,coords=mt$DATA,degree,polymodel)
# end_time<- Sys.time()
# end_time-start_time


###  Run transform with random data-Orginal realization of the stochastic process
cte<-as.matrix(sqrt(3) * 2 * (runif(n) - 1/2)) 
Q = 1 + M %*%(cte*eigenlambda)
par(mar = rep(2, 4))
# plot(x,Q,title(main='KL Realization'))
#plot is different since cte is random everytime

### Metrics
numvecs  <- ncol(Q)
maxerror=Inf
maxwaverror=Inf

#initialize indicator
n=1
polyerror<-list()
wavecoeffnorm<-list()

for (n in 1:numvecs){

  hbt<-hbtrans(Q[,n], mb$multileveltree, mb$ind, mb$datacell, mb$datalevel)
  
  Qv<-invhbtrans(hbt$dcoeffs, hbt$ccoeffs, mb$multileveltree, mb$ind, mb$datacell, mb$datalevel, numofpoints)
  
  polyerror[n]<- norm(Q[,n]-Qv, type='2') / norm(matrix(Q[,n]))
  wavecoeffnorm[n]<- norm(matrix(hbt$outputcoeff[1:(length(hbt$outputcoeff)-params$indexsetsize)]),type='i')/ norm(matrix(Q[,n]))
  #coeff == hbt$outputcoeff
  }

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

