# Julio Change Detection
# multilevelbasis.m
# 20201221 Xiaoyang Chen


# function [transformcell, ind, datacell, datalevel] = multilevelbasis(tree,coords,degree,polymodel);
multilevelbasis<-function(tree,coords,degree,polymodel){

# Construct Multi-Level basis in R^{d} described by random projection tree
# degree - Hyperbolic Cross, Total Degree, degree level
# Output HB basis in transformcell
  source('leaflocalbasis.R')
  source('localbasis.R')
  source('convertbintreetolist.R')
  
# Change tree format 
# [leftchild, rightchild, parent, datacell, datalevel] = convertbintreetolist(tree);
#return(list('leftchild'=leftchild,'rightchild'=rightchild,'parent'=parent,'datacell'=datacell,'datalevel'=datalevel,'node'=node))
  cl<-convertbintreetolist(mt$tree)
  
  # Sort datalevel
  tmpdatalevel<- sort(cl$datalevel,index.return=TRUE)
  sortdatalevel<-tmpdatalevel$x
  ind<-tmpdatalevel$ix
  
  #[dummy,unsortkey] = sort(ind)
  tmpind<-sort(ind,index.return=TRUE)
  dummy<-tmpind$x
  unsortkey<-tmpind$ix
  
  currentlevel<-tail(sortdatalevel,n=1)
  levelcounter<-length(sortdatalevel)
  

  
  # Data
  #transformcell = cell(length(ind),6);
  #transformcell<-matrix(list(),length(ind),6)
  
  transformcell<-matrix(list(),length(ind),6)
  degree=NaN
  # coords<-mt$DATA
  # polymodel
  
  # Process finest level first: currentlevel=5
  # leaflocalbasis(datacell{ind(levelcounter)}, coords, polymodel,degree);
  while (  sortdatalevel[levelcounter] == currentlevel  ) {
    llb<-leaflocalbasis(cl$datacell[[ind[levelcounter]]], coords=mt$DATA, polymodel,degree)
    # str(cl$datacell[63])
    # str(cl$datacell[[63]])
    
    transformcell[levelcounter,1]<-list(llb$Scfun)
    transformcell[levelcounter,2]<-list(llb$Wavefun)
    transformcell[levelcounter,3]<-list(llb$Cwave)
    transformcell[levelcounter,4]<-list(llb$Dwave)
    
    levelcounter=levelcounter - 1
  }
  
 
 
  minlevel = 1
  
  # process currentlevel=4,3,2,1
  while (currentlevel > minlevel ) {
    
    currentlevel = currentlevel-1
    #message('currentlevel',currentlevel)
    while ( sortdatalevel[levelcounter] == currentlevel ){
  
      if (   cl$leftchild[ind[levelcounter]] ==0 && cl$rightchild[ind[levelcounter]] ==0  ) {
       # Check if it is a leaf  
        llb<-leaflocalbasis(cl$datacell[[ind[levelcounter]]], coords=mt$DATA, polymodel,degree)
        transformcell[levelcounter,1]<-list(llb$Scfun)
        transformcell[levelcounter,2]<-list(llb$Wavefun)
        transformcell[levelcounter,3]<-list(llb$Cwave)
        transformcell[levelcounter,4]<-list(llb$Dwave)
      }
      
      else if (  cl$leftchild[ind[levelcounter]] !=0 && cl$rightchild[ind[levelcounter]] ==0  ) {
      # One child (left)  donothing
      transformcell[levelcounter,1] = transformcell[unsortkey[cl$leftchild[ind[levelcounter]]+1], 1]
      transformcell[levelcounter,2] = NaN
      
      transformcell[levelcounter,3] = transformcell[unsortkey[cl$leftchild[ind[levelcounter]]+1], 3]
      transformcell[levelcounter,4] = NaN
      # Clean not needed vectors 
      }
      
      else if (  cl$leftchild[ind[levelcounter]] ==0 && cl$rightchild[ind[levelcounter]] !=0  ) {
      # One child (right) donothing
      transformcell[levelcounter,1] = transformcell[unsortkey[cl$rightchild[ind[levelcounter]]+1], 1]
      transformcell[levelcounter,2] = NaN
      
      transformcell[levelcounter,3] = transformcell[unsortkey[cl$rightchild[ind[levelcounter]]+1], 3]
      transformcell[levelcounter,4] = NaN
      # Clean not needed vectors 
      } 
      
      else {
      # Two Children: both !=0
      #function [Scfun, Wavefun, Cwave, Dwave] = localbasis(Vleft, left_idxs, Vright, right_idxs, idxs, coords, polymodel, degree);
      lb<-localbasis(Vleft = transformcell[[unsortkey[cl$leftchild[ind[levelcounter]]+1], 1]],
                     left_idxs = cl$datacell[[cl$leftchild[ind[levelcounter]]+1]],
                     Vright = transformcell[[unsortkey[cl$rightchild[ind[levelcounter]]+1], 1]], 
                     right_idxs = cl$datacell[[cl$rightchild[ind[levelcounter]]+1]],
                     idxs = cl$datacell[[ind[levelcounter]]],
                     coords=mt$DATA, polymodel,degree)
      
      transformcell[levelcounter,1]<-list(lb$Scfun)
      transformcell[levelcounter,2]<-list(lb$Wavefun)
      transformcell[levelcounter,3]<-list(lb$Cwave)
      transformcell[levelcounter,4]<-list(lb$Dwave)
      }
      
      
      levelcounter=levelcounter - 1
      #message('inside levelcounter',levelcounter)
    }
 
  }
  
  # Transform last level == where currentlevel=0
  lblast<-localbasis(Vleft = transformcell[[unsortkey[cl$leftchild[ind[levelcounter]]+1], 1]],
                 left_idxs = cl$datacell[[cl$leftchild[ind[levelcounter]]+1]],
                 Vright = transformcell[[unsortkey[cl$rightchild[ind[levelcounter]]+1], 1]], 
                 right_idxs = cl$datacell[[cl$rightchild[ind[levelcounter]]+1]],
                 idxs = cl$datacell[[ind[levelcounter]]],
                 coords=mt$DATA, polymodel,degree)
  transformcell[levelcounter,1]<-list(lblast$Scfun)
  transformcell[levelcounter,2]<-list(lblast$Wavefun)
  transformcell[levelcounter,3]<-list(lblast$Cwave)
  transformcell[levelcounter,4]<-list(lblast$Dwave)
  
  
  
  # Add information of locations of basis functions in sparse matrix
  
  # Add level
  nmax<-nrow(transformcell)
  #mmax<-ncol(transformcell)
  for (n in 1:nmax) {
  transformcell[n,5] <- sortdatalevel[n]
  n=n+1
  }
  
  # Add location
  counterscaling = 0
  counterwave = 0
  transformcell<-cbind(transformcell,matrix(list(),length(ind),1)) #can we initiate 63 by 7 matrix at beginning?
  
  for (n in nmax:1){
  counterscaling <- counterscaling + ncol(transformcell[[n,1]])
  counterwave <- counterwave + ncol(transformcell[[n,2]])
  transformcell[n,6] <- counterscaling
  transformcell[n,7] <- counterwave
  n=n-1
  }
  
  
  
  #return(list('transformcell'=transformcell,'ind'=ind, 'datalevel'=cl$datalevel))
  return(list('transformcell'=transformcell,'ind'=ind, 'datacell'=cl$datacell, 'datalevel'=cl$datalevel))
}
