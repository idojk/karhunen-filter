data<-read.csv(file="Colon.txt", header=TRUE)

snapshot<-function(data,nk) 
{
  # Construct eigenfunctions by using eigenvectors
  cx <- data
  mx <- dim(cx)[2]
  
  # Compute eigenvectors and eigenvalues of covariance matrix
  k <- nk # number of eigenvalues

  #Run SVD to compute eigenvalues and eigenvectors
  #in matlab: svd-> s—Singular values,U—Left singular vectors,V-Right singular vectors
  #in R: cvd-> d-singular values, u-left singular vectors, v-right singular vectors
  C <- (t(cx) * cx) / mx
  dc<- svd(C)
  D <- dc$d
  U <- dc$u
  V <- dc$v
  
  # Normalize eigenfunction
  xefun <- t(U) * t(cx) / (sqrt(D)/sqrt(mx))
 
  results<-list(covariance=C, eigenvalue=D, eigenvector=U, eigenfunction=xefun)
}

snapshot_result<-snapshot(data,nk=21)
