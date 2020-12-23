# Julio Change Detection
# leaflocalbasis.m
# 20201222 Xiaoyang Chen

#function [Scfun, Wavefun, Cwave, Dwave] = leaflocalbasis(datapoints, coords, polymodel,degree)
leaflocalbasis<-function(datapoints, coords, polymodel,degree){
  
  # Create local Hierarchical Basis with vanishing moments
  # with respect to the polynomial model 
  
  coords<-coords[datapoints,] #depends on data structure
  n<-nrow(coords)
  m<-ncol(coords)
  
  
  # Initial basis 
  V<- diag(n)
  
  # Create local momement matrix
  Q<- PolynomialMonomials(degree, coords, polymodel, datapoints);
  M<- t(Q)%*%V
  
  # obtain HB basis
  #[Scfun, Wavefun, Cwave, Dwave] = computewave(M,V,n); 
  cw<-computewave(M,V,n)
  return(list('Scfun'=cw$Scfun, 'Wavefun'=cw$Wavefun, 'Cwave'=cw$Cwave, 'Dwave'=cw$Dwave))
}



