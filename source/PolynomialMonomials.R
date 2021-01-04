# Julio Change Detection
# PolynomialMonomials.m
# 20210102 Xiaoyang Chen

# function Q = PolynomialMonomials(degree, coords, polymodel,datapoints)
PolynomialMonomials<-function(degree, coords, polymodel,datapoints){
  
  
    # Compute vandemonde matrix for restricted polynomials
    # If design matrix is available then 
    if ( exists('M', where=polymodel) == TRUE ){
         Q <- polymodel$M[datapoints, ] 
         
         
    } else {
        # Compute reduced set monomials evaluated at coordinate knots
        # [n,dim] = size(coords);
        n<-nrow(coords)
        dim<-ncol(coords)
        
        # I = polymodel.fun(degree, dim, polymodel.restrictedtensor);
        I <- polymodel$indexset
        Qd <- t(I) 
        Q <-list()
        
        # Compute Vandermond matrix
        for ( i in 1:nrow(Qd) ){
        # TBD, M is in initialization in main file, so it should exist
        # B = repmat(Qd(i,:), [size(coords,1) 1])
        # C = coords.^B
        # Q = [Q prod(C')']
        }
    
    }
}