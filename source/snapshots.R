snapshots <- function(data) {
  
  # Construct eigenfunctions by using eigenvectors
  cx <- data
  mx <- dim(cx)[2]
  
  # Compute eigenvectors and eigenvalues of covariance matrix
  k <- 21 # number of eigenvalues
  
  #Run SVD to compute eigenvalues and eigenvectors
  C <- (t(cx) * cx) / M #Covariance matrix
  svd(C)
  s <- diag(u)
  s <- s(1:k)
  u <- d(1:k)
  
  results.covariance <- C
  results.eigenvalues <- s
  results.eigenvectors <- u
  
  # Normalize eigenfunction
  xefun <- t(results.eigenvectors) * t(cx) / (sqrt(s)/sqrt(mx))
  
  results.eigenfunction <- xefun
}
