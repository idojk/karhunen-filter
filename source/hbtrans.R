# Julio Change Detection
# hbtrans.m
# 20210113 Xiaoyang Chen
 

# Multilevel transform;
# Performs projection of the data fdata to the multilevel
# basis in transformcall

# function [outputcoeff, levelcoeff, dcoeffs, ccoeffs] = hbtrans(fdata, transformcell, ind, datacell, datalevel)
hbtrans<-function(fdata, transformcell, ind, datacell, datalevel) {

  # Projection
  numofnodes = nrow(transformcell)
  coeff = vector()
  levelcoeff = vector()
  dcoeffs = matrix(list(),numofnodes,1)
  ccoeffs = vector()
  
  
  # Up to level 0 of HB coefficients
  for (n in numofnodes:1 ) {
  # if is not empty 
  if (  is.null(transformcell[n,2]) == FALSE  ) {
      ixds = datacell[[ind[n]]]
      W = transformcell[[n,2]] 
      localcoeff = t(fdata[ixds]) %*% W
      coeff <- c(coeff,localcoeff)
      levelcoeff <- c(  levelcoeff, rep( datalevel[ind[n]],ncol(W) )  )
      dcoeffs[n] <- list(localcoeff)
      n=n-1
      }
  }
  
  
  # Perform zero last level
  if (  is.null(transformcell[1,1]) == FALSE  ) {
      V <- transformcell[[1,1]]
      coeff <- c(coeff, t(fdata) %*% V)
      ccoeffs <- t(fdata) %*% V
      levelcoeff <- c(  levelcoeff,rep(-1,ncol(V))  )
      coeff <-t(coeff)
      }
  
 
  nfdata = length(fdata)
  outputcoeff = rep(0,nfdata)
  outputcoeff[1:length(coeff)] <- coeff

 
  return(list('outputcoeff'=outputcoeff, 'levelcoeff'=levelcoeff, 'dcoeffs'=dcoeffs, 'ccoeffs'=ccoeffs))
}
