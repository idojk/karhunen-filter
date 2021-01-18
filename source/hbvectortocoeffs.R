# Julio Change Detection
# hbvectortocoeffs.m
# 20210117 Xiaoyang Chen


#as.matrix(a,b) <- hbvectortocoeffs(hbt$outputcoeff, multileveltree, ind, datacell, datalevel, numofpoints)
#function [dcoeffs, ccoeffs] = hbvectortocoeffs(coeffsvector, multileveltree, ind, datacell, datalevel, numofpoints)
hbvectortocoeffs<-function(coeffsvector, multileveltree, ind, datacell, datalevel, numofpoints) {
  # Extraction utility that converts coefficient vector
  # format to struct coefficient vector
  
  
  # Projection
  numofnodes = nrow(multileveltree)
  coeff = vector()
  levelcoeff = vector()
  dcoeffs = matrix(list(),numofnodes,1)
  ccoeffs = vector()
  
  
  # Up to level 0 of HB coefficients
  indexvector = 1
  for (n in numofnodes:1 ){
    #if isempty(multileveltree{n,2}) == 0
      lengthlocalcoeffs <- ncol(multileveltree[[n,2]])
      indexvector <- c(  indexvector, (indexvector[length(indexvector)] + lengthlocalcoeffs)   )
      # if isempty(multileveltree{n,2}) == 1; keyboard; end
      # dcoeffs{n} = coeffsvector();
      n=n-1
    #}
  }
  
  for (n in 1:numofnodes) {
    # disp(n)
    #if isempty(multileveltree{n,2}) == 0 
    dcoeffs[numofnodes - n + 1] <- list(  coeffsvector[indexvector[n]:(indexvector[n+1]-1)]  )
    #}
    }
  
  
  # Last level
  ccoeffs<-coeffsvector[indexvector[length(indexvector)] : length(coeffsvector)]


  return(list('dcoeffs'= dcoeffs, 'ccoeffs'=ccoeffs))
}

