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

start_time<- Sys.time()
for (n in 1:numvecs){

  hbt<-hbtrans(Q[,n], mb$multileveltree, mb$ind, mb$datacell, mb$datalevel)
  
  Qv<-invhbtrans(hbt$dcoeffs, hbt$ccoeffs, mb$multileveltree, mb$ind, mb$datacell, mb$datalevel, numofpoints)
  
  polyerror[n]<- norm(Q[,n]-Qv, type='2') / norm(matrix(Q[,n]))
  
  wavecoeffnorm[n]<- norm(matrix(hbt$outputcoeff[1:(length(hbt$outputcoeff)-params$indexsetsize)]),type='i')/ norm(matrix(Q[,n]))
  #coeff == hbt$outputcoeff
  }
end_time<- Sys.time()
message('Total timing=   ',end_time-start_time)

message('Results: Num of tests =   ', numvecs)
message('Max Relative Error =   ', max(unlist(polyerror)))
message('Relative HBcoefficientsnorm =   ',max(unlist(wavecoeffnorm)))
#test one hbtrans running
start_time<- Sys.time()
hbttime<- hbtrans(Q[,n], mb$multileveltree, mb$ind, mb$datacell, mb$datalevel)
end_time<- Sys.time()
message('One Hierarchical Basis transform time=   ',end_time-start_time)


#Test format change from vector of coefficients to struct
totalerror=0
hbcoef <- hbvectortocoeffs(hbt$outputcoeff,mb$multileveltree,mb$ind,mb$datacell,mb$datalevel,numofpoints)
a<-hbcoef$dcoeffs
b<-hbcoef$ccoeffs
for (i in 1:length(hbcoef$dcoeffs)){
  totalerror <- totalerror + max(svd( matrix(a[[i]]-hbcoef$dcoeffs[[i]]) )$d)
  }
totalerror <- totalerror + max(svd( matrix(b-hbcoef$ccoeffs) )$d)
message('Total error = ', totalerror)



### Plot coefficients
library(ggplot2)
library(gridExtra)

#figure(2)
maxlevel = max(hbt$levelcoeff)
numlevel = 3
counter = 1
n=1
# par(mar = rep(2, 4))
plot2<-matrix(list(),1,5)
plot2[[1]]<-ggplot(data.frame(x,Q), aes(x=x, y=Q))+geom_point()+ggtitle('KL Realization')+ theme_light()             
# print(plot2[[1]])

for (n in maxlevel:(maxlevel-numlevel)){
  counter<-counter + 1
  
  plot2data<-data.frame(px=1:length(hbt$levelcoeff[hbt$levelcoeff == n]),py=hbt$outputcoeff[hbt$levelcoeff == n])
  plot2[[counter]]<-ggplot(plot2data, aes(x=px, y=py))+
  geom_segment( aes(x=px, xend=px, y=0, yend=py), color="grey")+
  geom_point(color="blue", size=2)+
  theme_light()+
  ggtitle(paste("Level Coefficients = ",n))
  print(plot2[[counter]])
  
  n=n-1
}

png(filename = "tkt_plot2.png", width = 800, height = 1200, units = "px")
grid.arrange(plot2[[1]],plot2[[2]],plot2[[3]],plot2[[4]],plot2[[5]],ncol=1)
dev.off()



### Run trans
print('Add Bump to KL ')
#Random realization
#Q = polymodel.M * rand(size(polymodel.M,2),1);
###Add Gaussian "bump" to the data
sbump = 0.5
Qkl = Q
sigma = 0.001
maxbump = 0.05
bump <- maxbump * exp(- ((t(x) - sbump)^2) /sigma)
bump[1:150] <- 0
bump[350:length(bump)] <- 0
Q <- Q + t(bump)

numvecs = ncol(Q)
maxerror=Inf
maxwaverror=Inf
for (n in 1:numvecs){
  hbt<-hbtrans(Q[,n], mb$multileveltree, mb$ind, mb$datacell, mb$datalevel)
  
  Qv<-invhbtrans(hbt$dcoeffs, hbt$ccoeffs, mb$multileveltree, mb$ind, mb$datacell, mb$datalevel, numofpoints)
  
  polyerror[n]<- norm(Q[,n]-Qv, type='2') / norm(matrix(Q[,n]))
  
  wavecoeffnorm[n]<- norm(matrix(hbt$outputcoeff[1:(length(hbt$outputcoeff)-params$indexsetsize)]),type='i')/ norm(matrix(Q[,n]))
}


#figure(3)
plot3<-matrix(list(),1,3)

plot3[[1]]<-ggplot(data.frame(x,Qkl), aes(x=x, y=Qkl))+geom_line()+theme_light()+
            ggtitle('KL Realization without Gaussian bump')            
# print(plot3[[1]])
plot3y2<-maxbump * exp(- ((t(x) - sbump)^2) /sigma)
plot3[[2]]<-ggplot(data.frame(x,plot3y2), aes(x=x, y=plot3y2))+geom_line()+theme_light()+
            ggtitle('Gaussian bump')   
plot3[[3]]<-ggplot(data.frame(x,Q), aes(x=x, y=Q))+geom_line()+theme_light()+
            ggtitle('KL Realization with Gaussian bump')  

png(filename = "tkt_plot3.png", width = 480, height = 800, units = "px")
grid.arrange(plot3[[1]],plot3[[2]],plot3[[3]],ncol=1)
dev.off()


#figure(4)
maxlevel = max(hbt$levelcoeff)
numlevel = 5
counter = 1
n=1
plot4<-matrix(list(),1,numlevel)
