# Julio Change Detection
# convertbintreetolist.m
# 20201221 Xiaoyang Chen


# function  [leftchild,rightchild,parent,datacell,datalevel] = convertbintreetolist(tree);
# Invoke: cl<-convertbintreetolist(mt$tree)
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
  parentnode = 0
  
  
  
  #[leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree,datalevel,node);
  cdt<-createdata(leftchild,rightchild,parent,datacell,tree,datalevel,node)
  
  return(list('leftchild'=cdt$leftchild,'rightchild'=cdt$rightchild,'parent'=cdt$parent,'datacell'=cdt$datacell,'datalevel'=cdt$datalevel))
  
}



#function [leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree,datalevel,node);
createdata<-function(leftchild,rightchild,parent,datacell,tree,datalevel,node){
  
  datacell[node + 1]<- list(tree$idxs)
  datalevel[node + 1]<- tree$currentdepth
  # str(mt$tree$idxs)
  # str(datacell[node + 1])
  
  
  # if is not empty
  if (is.null(tree$left) != TRUE){
    leftchild[node + 1]<- tree$childleft
    }
  
  if (is.null(tree$right) != TRUE){
    rightchild[node + 1]<- tree$childright
    }
  
  if (node>0) {
    parent[node + 1]<-tree$parentnode
    }
  
  
  
  #increase the node
  node = node + 1
  
  
  # if both child is null
  if (  (is.null(tree$left)) && (is.null(tree$right)) ){
    return(list('leftchild'=leftchild,'rightchild'=rightchild,'parent'=parent,'datacell'=datacell,'datalevel'=datalevel,'node'=node))
  }
  
  
  
  # [leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree.left,datalevel,node);
  createdataleft<-createdata(leftchild,rightchild,parent,datacell,tree$left,datalevel,node)
  leftchild<-createdataleft$leftchild
  rightchild<-createdataleft$rightchild
  parent<-createdataleft$parent
  datacell<-createdataleft$datacell
  datalevel<-createdataleft$datalevel
  node<-createdataleft$node

  
  # [leftchild,rightchild,parent,datacell,datalevel,node] = createdata(leftchild,rightchild,parent,datacell,tree.right,datalevel,node);
  createdataright<-createdata(leftchild,rightchild,parent,datacell,tree$right,datalevel,node)
  leftchild<-createdataright$leftchild
  rightchild<-createdataright$rightchild
  parent<-createdataright$parent
  datacell<-createdataright$datacell
  # tree$right<-createdataleft$tree$right
  datalevel<-createdataright$datalevel
  node<-createdataright$node
  
  
  
  return(list('leftchild'=leftchild,'rightchild'=rightchild,'parent'=parent,'datacell'=datacell,'datalevel'=datalevel,'node'=node))
}

