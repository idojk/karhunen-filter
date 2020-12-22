# Julio Change Detection
# convertbintreetolist.m
# 20201221 Xiaoyang Chen

# simulation
# sizeoftree=63


# function  [leftchild,rightchild,parent,datacell,datalevel] = convertbintreetolist(tree);
# Transform tree to list data
convertbintreetolist<-function(tree){
  
  # Find tree size
  sizeoftree<-tree$size
  
  # Initialize cell array
  leftchild<-rep(0,sizeoftree)
  rightchild<-rep(0,sizeoftree)
  parent<-rep(0,sizeoftree)
  datalevel<-rep(0,sizeoftree)
  datacell<-matrix(list(),sizeoftree,1)
  # str(datacell)
  # str(datalevel)
  
  # Initialize
  node = 0
  #parentnode(1) = 0
  parentnode = 0
  
  #[leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree,datalevel,node);
  createdataout<-createdata(leftchild,rightchild,parent,datacell,tree,datalevel,node)
}


#function [leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree,datalevel,node);
createdata<-function(leftchild,rightchild,parent,datacell,tree,datalevel,node){

datacell[mt$tree$node + 1]<- mt$tree$idxs
datalevel[mt$tree$node + 1]<- mt$tree$currentdepth

if (is.null(mt$tree$left) != TRUE){
  leftchild[mt$tree$node + 1]<- mt$tree$childleft
  }

if (is.null(mt$tree$right) != TRUE){
  rightchild[mt$tree$node + 1]<- mt$tree$childright
  }

if (node>0) {
  parent[mt$tree$node + 1]<-mt$tree$parentnode
  }


node = node + 1

# if both child is null
if (  (is.null(mt$tree$left)) && (is.null(mt$tree$right)) ){
  return(list(   ))
}


# [leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree.left,datalevel,node);
# [leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree.right,datalevel,node);
createdataleft<-createdata(leftchild,rightchild,parent,datacell,tree.left,datalevel,node)
createdataright<-createdata(leftchild,rightchild,parent,datacell,tree.right,datalevel,node)
}

