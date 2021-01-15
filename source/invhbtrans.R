# Julio Change Detection
# invhbtrans.m
# 20210114 Xiaoyang Chen

#function fdata = invhbtrans(dcoeffs, ccoeffs, transformcell, ind, datacell, datalevel, numofpoints)

invhbtrans<-function(dcoeffs, ccoeffs, transformcell, ind, datacell, datalevel, numofpoints){
  
  #Inverse Multilevel transform;
  #Reconstruct fdata from coeff and the multilevel
  #basis in transformcall
  
  #Reconstruction
  numofnodes <- nrow(transformcell)
  fdata <- rep(0,numofpoints)
  
  
  #Up to level 0 of HB coefficients
  
  for (  n in numofnodes:1 )  {
    if (  is.null(transformcell[n,2]) == FALSE  ) {
    
        ixds <- datacell[[ind[n]]]
        W <- transformcell[[n,2]]
        
        fdata[ixds] = fdata[ixds] + W%*%t(dcoeffs[[n]])
        }  
    n=n-1
    }
  
  
  #Perform zero last level
  V <- transformcell[[1,1]]
  fdata <- fdata +  V%*%t(ccoeffs)

  
  return(fdata)
}
