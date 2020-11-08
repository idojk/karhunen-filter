setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")

data<-read.csv(file="Colon.txt", header=TRUE)
snapshot<-function(data,k) 
{
  # Construct eigenfunctions by using eigenvectors
  cx <- data
  mx <- dim(cx)[2]
  k=21
  # Compute eigenvectors and eigenvalues of covariance matrix

  #Run SVD to compute eigenvalues and eigenvectors
  #in matlab: svd-> s-Singular values,U-Left singular vectors,V-Right singular vectors
  #in R: cvd-> d-singular values, u-left singular vectors, v-right singular vectors
  C <- (t(cx) * cx) / mx
  dc<- svd(C)
  D <- (diag(dc$d))[1:k,] #take row 1 to row k of diagnal matrix of D
  #take all 62 cols which is all obs of a cell?
  #D <- dc$d
  #diagD<-diag(D)
  #partD<-diagD[1:k,] 

  U <- dc$u[,1:k] # take 1 to k columns of left singular matrix
  V <- dc$v
  
  # Normalize eigenfunction
  xefun <- t(U) * t(cx) / (sqrt(D)/sqrt(mx))
 
  results<-list(covariance=C, eigenvalue=D, eigenvector=U, eigenfunction=xefun)
}

snapshot_result<-snapshot(data,k=21)
