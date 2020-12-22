# Julio Change Detection
# multilevelbasis.m
# 20201221 Xiaoyang Chen

# function [transformcell, ind, datacell, datalevel] = multilevelbasis(tree,coords,degree,polymodel);
multilevelbasis<-function(tree,coords,degree,polymodel){

# Construct Multi-Level basis in R^{d} described by random projection tree
# degree - Hyperbolic Cross, Total Degree, degree level
# Output HB basis in transformcell

# Change tree format 
# [leftchild, rightchild, parent, datacell, datalevel] = convertbintreetolist(tree);
  cl<-convertbintreetolist(tree)
  
  # Sort datalevel
  [sortdatalevel, ind] <- sort(datalevel)
  [dummy,unsortkey] = sort(ind);
  currentlevel = sortdatalevel(end); 
  levelcounter = size(sortdatalevel,1);
  
  return(list('transformcell'=transformcell,'ind'=ind, 'datacell'=datacell, 'datalevel'=datalevel))
}