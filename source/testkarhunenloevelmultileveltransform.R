# Julio Change Detection
# testkarhunenloevelmultileveltransform.m
# 20201107 Xiaoyang Chen

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
source("make_tree.R")
make_tree

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
