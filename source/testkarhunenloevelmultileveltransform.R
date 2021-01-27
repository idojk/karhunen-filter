# Julio Change Detection
# testkarhunenloevelmultileveltransform.m
# 20201107 Xiaoyang Chen


# options(buildtools.check = function(action) TRUE )
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("dbplyr")
# install.packages("hms")
# install.packages("pillar")
# install.packages("rlang")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages('patchwork')
##check one package version
# packageVersion('')
##check all packages
# sessionInfo()
# library(patchwork)

library(ggplot2)
library(gridExtra)

`%notin%` <- Negate(`%in%`)
setwd("H:/2021Sep/Julio/ChangeDetection/ChangeDetectionR")
source("initializationVec.R") #comment it when debugging
source("make_tree.R") 
source("multilevelbasis.R")
source("hbtrans.R")
source("invhbtrans.R")
source("hbvectortocoeffs.R")



###Data and parameters
ini<-initializationVec(numofpoints=500)



### Create Multilevel Binary tree
# start_time<- Sys.time()
mt<-make_tree(ini$data,split_KD,ini$params) #split_KD to be update
# end_time<- Sys.time()
# end_time-start_time
## check results
# str(mt)
# level5<-mt$tree$left$left$left$left$left
# mt[['tree']][['left']][['left']][['left']][['left']][['left']][['idxs']]
# mt[['tree']][['left']][['left']][['left']][['left']][['right']][['idxs']]
# right should be 17:32



###  Create multilevel basis
# start_time<- Sys.time()
mb<-multilevelbasis(mt$tree,coords=mt$DATA,ini$degree,ini$polymodel)
# end_time<- Sys.time()
# end_time-start_time





#################  Plotting starts  ########################

###  Run transform with random data-Orginal realization of the stochastic process
cte<-as.matrix(sqrt(3) * 2 * (runif(ini$n) - 1/2)) 
Q = 1 + ini$polymodel$M %*%(cte*ini$eigenlambda)
# png(filename = "tkt_plot1.png", width = 400, height = 300, units = "px")
ggplot(data.frame(ini$x,Q), aes(x=ini$x, y=Q))+geom_line()+theme_light()+ggtitle('KL Realization')    
# dev.off()
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
  
  Qv<-invhbtrans(hbt$dcoeffs, hbt$ccoeffs, mb$multileveltree, mb$ind, mb$datacell, mb$datalevel, ini$numofpoints)
  
  polyerror[n]<- norm(Q[,n]-Qv, type='2') / norm(matrix(Q[,n]))
  
  wavecoeffnorm[n]<- norm(matrix(hbt$outputcoeff[1:(length(hbt$outputcoeff)-ini$params$indexsetsize)]),type='i')/ norm(matrix(Q[,n]))
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
hbcoef <- hbvectortocoeffs(hbt$outputcoeff,mb$multileveltree,mb$ind,mb$datacell,mb$datalevel,ini$numofpoints)
a<-hbcoef$dcoeffs
b<-hbcoef$ccoeffs
for (i in 1:length(hbcoef$dcoeffs)){
  totalerror <- totalerror + max(svd( matrix(a[[i]]-hbcoef$dcoeffs[[i]]) )$d)
  }
totalerror <- totalerror + max(svd( matrix(b-hbcoef$ccoeffs) )$d)
message('Total error = ', totalerror)




### Plot coefficients
#figure(2)
maxlevel = max(hbt$levelcoeff)
numlevel = 3
counter = 1
n=1
# par(mar = rep(2, 4))
plot2<-matrix(list(),1,5)
plot2[[1]]<-ggplot(data.frame(ini$x,Q), aes(x=ini$x, y=Q))+geom_line()+ggtitle('KL Realization')+ theme_light()             
# print(plot2[[1]])

for (n in maxlevel:(maxlevel-numlevel)){
  counter<-counter + 1
  
  plot2data<-data.frame(px=1:length(hbt$levelcoeff[hbt$levelcoeff == n]),py=hbt$outputcoeff[hbt$levelcoeff == n])
  plot2[[counter]]<-ggplot(plot2data, aes(x=px, y=py))+
  geom_segment( aes(x=px, xend=px, y=0, yend=py), color="grey")+
  geom_point(color="blue", size=2)+
  theme_light()+
  ggtitle(paste("Level Coefficients = ",n))
  # print(plot2[[counter]])
  
  n=n-1
}

# png(filename = "tkt_plot2.png", width = 800, height = 1200, units = "px")
grid.arrange(plot2[[1]],plot2[[2]],plot2[[3]],plot2[[4]],plot2[[5]],ncol=1)
# dev.off()



### Run trans
print('Add Bump to KL ')
#Random realization
#Q = polymodel.M * rand(size(polymodel.M,2),1);
###Add Gaussian "bump" to the data
sbump = 0.5
Qkl = Q
sigma = 0.001
maxbump = 0.05
bump <- maxbump * exp(- ((t(ini$x) - sbump)^2) /sigma)
bump[1:150] <- 0
bump[350:length(bump)] <- 0
Q <- Q + t(bump)

# matlab line 157-164    why do we need this, besides of timing? It will overwrite previous results.
numvecs = ncol(Q)
maxerror=Inf
maxwaverror=Inf
for (n in 1:numvecs){
  hbt<-hbtrans(Q[,n], mb$multileveltree, mb$ind, mb$datacell, mb$datalevel)
  
  Qv<-invhbtrans(hbt$dcoeffs, hbt$ccoeffs, mb$multileveltree, mb$ind, mb$datacell, mb$datalevel, ini$numofpoints)
  
  polyerror[n]<- norm(Q[,n]-Qv, type='2') / norm(matrix(Q[,n]))
  
  wavecoeffnorm[n]<- norm(matrix(hbt$outputcoeff[1:(length(hbt$outputcoeff)-ini$params$indexsetsize)]),type='i')/ norm(matrix(Q[,n]))
}



#figure(3)
plot3<-matrix(list(),1,3)

plot3[[1]]<-ggplot(data.frame(ini$x,Qkl), aes(x=ini$x, y=Qkl))+geom_line()+theme_light()+
            ggtitle('KL Realization without Gaussian bump')            
# print(plot3[[1]])
plot3y2<-maxbump * exp(- ((t(ini$x) - sbump)^2) /sigma)
plot3[[2]]<-ggplot(data.frame(ini$x,plot3y2), aes(x=ini$x, y=plot3y2))+geom_line()+theme_light()+
            ggtitle('Gaussian bump')   
plot3[[3]]<-ggplot(data.frame(ini$x,Q), aes(x=ini$x, y=Q))+geom_line()+theme_light()+
            ggtitle('KL Realization with Gaussian bump')  

# png(filename = "tkt_plot3.png", width = 480, height = 800, units = "px")
grid.arrange(plot3[[1]],plot3[[2]],plot3[[3]],ncol=1)
# dev.off()



#figure(4)
maxlevel = max(hbt$levelcoeff)
numlevel = 5
counter = 1
n=1
plot4<-matrix(list(),1,numlevel)
plot4[[1]]<-ggplot(data.frame(ini$x,Q,Qkl,plot3y2), aes(x=ini$x))+theme_light()+
  geom_line(aes(y=Q),color='blue')+
  geom_line(aes(y=Qkl),linetype='dashed')+
  geom_line(aes(y=plot3y2),color='orange')


outputcell<-matrix(list(),numlevel+2,2)
outputcell[1]<-list(t(ini$x))
outputcell[2]<-list(plot3y2)

for (n in maxlevel:(maxlevel-numlevel)){
  counter<-counter + 1
  
  plot4data<-data.frame(px=1:length(hbt$levelcoeff[hbt$levelcoeff == n]),py=hbt$outputcoeff[hbt$levelcoeff == n])
  plot4[[counter]]<-ggplot(plot4data, aes(x=px, y=py))+
    geom_segment( aes(x=px, xend=px, y=0, yend=py), color="grey")+
    geom_point(color="blue", size=2)+
    theme_light()+
    ggtitle(paste("Level Coefficients = ",n))
  # print(plot4[[counter]])
  
  outputcell[counter+1]<-list(hbt$levelcoeff[hbt$levelcoeff == n])
  
  n=n-1
}

# png(filename = "tkt_plot4.png", width = 800, height = 1600, units = "px")
grid.arrange(plot4[[1]],plot4[[2]],plot4[[3]],plot4[[4]],plot4[[5]],plot4[[6]],plot4[[7]],ncol=1)
# dev.off()
