# Julio Change Detection
# multilevelbasis.m
# 20201221 Xiaoyang Chen

#simulate data
# ind<-1:63


# function [transformcell, ind, datacell, datalevel] = multilevelbasis(tree,coords,degree,polymodel);
multilevelbasis<-function(tree,coords,degree,polymodel){

# Construct Multi-Level basis in R^{d} described by random projection tree
# degree - Hyperbolic Cross, Total Degree, degree level
# Output HB basis in transformcell

# Change tree format 
# [leftchild, rightchild, parent, datacell, datalevel] = convertbintreetolist(tree);
#return(list('leftchild'=leftchild,'rightchild'=rightchild,'parent'=parent,'datacell'=datacell,'datalevel'=datalevel,'node'=node))
  cl<-convertbintreetolist(tree)
  
  # Sort datalevel
  tmpdatalevel<- sort(datalevel,return.index=TRUE)
  sortdatalevel<-tmpdatalevel$x
  ind<-tmpdatalevel$ix
  
  [dummy,unsortkey] = sort(ind)
  tmpind<-sort(inc,return.index=TRUE)
  dummy<-tmpind$x
  unsortkey<-tmpind$ix
  
  currentlevel<-tail(sortdatalevel,n=1)
  levelcounter<-nrow(sortdatalevel)
  

  # Data
  #transformcell = cell(length(ind),6);
  transformcell<-matrix(NaN,length(ind),6)
  
  
  # Process finest level first
  while (  sortdatalevel(levelcounter) == currentlevel  ) {
    llb<-leaflocalbasis(datacell{ind(levelcounter)}, coords, polymodel,degree)
    transformcell[levelcounter,1]<-llb$Scfun
    transformcell[levelcounter,2]<-llb$Wavefun
    transformcell[levelcounter,3]<-llb$Cwave
    transformcell[levelcounter,4]<-llb$Dwave
    
    levelcounter=levelcounter - 1
  }
  
 
 
  minlevel = 1
  
  while (currentlevel > minlevel ) {
    currentlevel <- currentlevel - 1

    while ( sortdatalevel[levelcounter] == currentlevel ){
  
      if (leftchild(ind[levelcounter]) == 0 && rightchild(ind[levelcounter]) == 0) {
       # Check if it is a leaf  
        llb<-leaflocalbasis(cl$datacell[[ind[levelcounter]]], coords=mt$DATA, polymodel,degree)
        transformcell[levelcounter,1]<-list(llb$Scfun)
        transformcell[levelcounter,2]<-list(llb$Wavefun)
        transformcell[levelcounter,3]<-list(llb$Cwave)
        transformcell[levelcounter,4]<-list(llb$Dwave)
        # 1/6 stop here
      }
      
      else if (leftchild$ind[levelcounter] != 0 && rightchild$ind[levelcounter] == 0) {
      # One child (left)  donothing
      transformcell[levelcounter,1] = transformcell[unsortkey(leftchild(ind[levelcounter]) + 1), 1]
      transformcell[levelcounter,2] = NaN
      
      transformcell[levelcounter,3] = transformcell[unsortkey(leftchild(ind[levelcounter]) + 1), 3]
      transformcell[levelcounter,4] = NaN
      # Clean not needed vectors 
      }
      
      else if (leftchild$ind[levelcounter] == 0 && rightchild$ind[levelcounter] != 0) {
      # One child (right) donothing
      transformcell[levelcounter,1] = transformcell[unsortkey(rightchild(ind[levelcounter]) + 1), 1]
      transformcell[levelcounter,2] = NaN
      
      transformcell[levelcounter,3] = transformcell[unsortkey(rightchild(ind[levelcounter]) + 1), 3]
      transformcell[levelcounter,4] = NaN
      
      # Clean not needed vectors 
      } else {
      
      # Two Children
      }
      
      
      levelcounter=levelcounter - 1
      }
  # Transform last level
  # Add information of locations of basis functions in sparse matrix
  }
  
  
  
  
  return(list('transformcell'=transformcell,'ind'=ind, 'datacell'=datacell, 'datalevel'=datalevel))
}
