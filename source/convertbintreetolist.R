# Julio Change Detection
# convertbintreetolist.m
# 20201221 Xiaoyang Chen

# function  [leftchild,rightchild,parent,datacell,datalevel] = convertbintreetolist(tree);
# Transform tree to list data
convertbintreetolist<-function(tree){
  
  # Find tree size
  sizeoftree<-tree$size;
  
  # Initialize cell array
  leftchild  = zeros(sizeoftree, 1);
  rightchild = zeros(sizeoftree, 1);
  parent     = zeros(sizeoftree, 1);
  datalevel  = zeros(sizeoftree, 1);
  
  datacell   = cell(sizeoftree,  1);
}