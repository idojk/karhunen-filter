snapshots <- function(data) {
 
data<-read.csv(file="Colon.txt", header=TRUE)
  
  # Construct eigenfunctions by using eigenvectors
  cx <- data
  mx <- dim(cx)[2]
  
  # Compute eigenvectors and eigenvalues of covariance matrix
  k <- 21 # number of eigenvalues
  M<-matrix(1,mx,mx)
  #Run SVD to compute eigenvalues and eigenvectors
  C <- (t(cx) * cx) / M #Covariance matrix
  s<-svd(C)
  D <- s$d
  U <- s$u
  V <- s$v
  
  results.covariance <- C
  results.eigenvalues <- s
  results.eigenvectors <- u
  
  # Normalize eigenfunction
  xefun <- t(C) * t(cx) / (sqrt(U)/sqrt(mx))
  
  results.eigenfunction <- xefun
}
