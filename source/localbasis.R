# Julio Change Detection
# localbasis.m
# 20210109 Xiaoyang Chen

#function [Scfun, Wavefun, Cwave, Dwave] = localbasis(Vleft, left_idxs, Vright, right_idxs, idxs, coords, polymodel, degree);
localbasis<-function(Vleft, left_idxs, Vright, right_idxs, idxs, coords, polymodel, degree){


# Create local Hierarchical Basis with vanishing moments
# with respect to the polynomial model. Use vectors from
# previous level on the tree
coords<-as.matrix(coords[idxs,])
n<-nrow(coords)
m<-ncol(coords)

# Initial basis 
V<-rbind(    cbind(  Vleft,matrix(0,nrow=nrow(Vleft), ncol=ncol(Vright))  ),
             cbind(  matrix(0,nrow=nrow(Vright), ncol=ncol(Vleft)),Vright  )    )

# sort V
tmplridx <- sort(c(left_idxs,right_idxs),index.return=TRUE)
key<-tmplridx$ix
V<-V[key,]

# Create local momement matrix
# Q = PolynomialMonomials(degree, coords, polymodel,idxs);
setwd("H:/2021Sep/Julio/ChangeDetection/ChangeDetectionR")
source('PolynomialMonomials.R')
Q<- PolynomialMonomials(degree, coords, polymodel, idxs)
M<- t(Q)%*%V

# obtain HB basis
#[Scfun, Wavefun, Cwave, Dwave] = computewave(M,V,n);
source('computewave.R')
cw<-computewave(M,V,n)
return(list('Scfun'=cw$Scfun, 'Wavefun'=cw$Wavefun, 'Cwave'=cw$Cwave, 'Dwave'=cw$Dwave))

}
