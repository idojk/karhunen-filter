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
  
  
  
  return(list('transformcell'=transformcell,'ind'=ind, 'datacell'=datacell, 'datalevel'=datalevel))
}
