# Julio Change Detection
# initialization of data: 1:500
# 20210126 Xiaoyang Chen


initializationVec<-function(numofpoints){
  
  degree <-matrix(NaN,1,1)
  # numofpoints <-500
  
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
  
  
  
  return(list('data'=data,'params'=params,'polymodel'=polymodel,'degree'=degree,'n'=n,
              'eigenlambda'=eigenlambda,'x'=x,'numofpoints'=numofpoints))
}
