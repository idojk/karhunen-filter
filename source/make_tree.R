# Julio Change Detection
# make_tree.m
# 20201029 Xiaoyang Chen
# clean backup
# To be done next: update path after finishing all modules



#start
`%notin%` <- Negate(`%in%`)

### function [tree,DATA] = make_tree(DATA,split_function,params)
make_tree<-function(DATA,split_function,params){
  
  #define nargin in R
  nargin <- length(as.list(match.call())) -1
 
   
  ### define make tree function
  if (nargin<2){
    split_function='split_PCA'
  }
  if (nargin<3){params<-list()}
  if ('MAX_DEPTH' %notin% params){params$MAX_DEPTH=15}
  if ('split_fxn_params' %notin% params){   
    spill <-0
    split_fxn_params<-list(spill)
    params$split_fxn_params<-list(split_fxn_params)
  }

    
  #Initialize first node
  node=0
  
  
  # Create tree
  #[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
  create_tree_out<-create_tree(DATA,1:nrow(DATA),split_function,params$indexsetsize,params$split_fxn_params, params$MAX_DEPTH,1,node)
  create_tree_out$tree$size <- create_tree_out$node
  return(list('tree'=create_tree_out$tree, 'DATA'=create_tree_out$data))
}




### define create_tree function

### [tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
create_tree<-function(DATA,idxs,split_function,indexsetsize,split_fxn_params, MAX_DEPTH,curr_depth,node){
  setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\") #change to package path

  
  # initialize
  tree<-list()  #initialize null dataframe
  #tree$idxs <-list(idxs)
  tree$idxs <-idxs
  mx <- dim(DATA)[1]
  tree$threshold = NaN
  tree$split_dir = NaN
  tree$proj_data =  matrix(NaN,1,mx)
  tree$center = mean(DATA[idxs,])
  tree$idxsmax = max(DATA[idxs,])
  tree$idxsmin = min(DATA[idxs,])
  
  
  # Add depth to each node
  tree$currentdepth = curr_depth - 1 
  
  # Add node numbering
  tree$node = node
  parentnode = node #current node is the parent node of two branches
  
  
  # increase node
  node = node + 1
  #cellinfo <-data.frame(matrix(ncol = 1, nrow = 1))
  
  
  if (isTRUE(curr_depth>=MAX_DEPTH)) {
    return(list("tree"=tree,"node"=node,"DATA"=data))
  }
  if (isTRUE( length(idxs)<=ceiling(indexsetsize + 1) )) {
    return(list("tree"=tree,"node"=node,"DATA"=data))
  }
  
  
  
  #[idx_left, idx_right, threshold, split_dir, proj_data] = split_function(DATA(idxs,:), split_fxn_params);
  source('split_KD.R')
  sp<-split_KD(as.matrix(DATA[idxs,]), split_fxn_params)
  
  #overwrite left_idxs and right_idxs for next layer
  left_idxs <- idxs*sp$left_idx
  left_idxs<-left_idxs[left_idxs!=0]
  right_idxs <- idxs*sp$right_idxs
  right_idxs<-right_idxs[right_idxs!=0]
  
  
  
  # Test
  if (  (length(left_idxs)< indexsetsize) || (length(right_idxs) < indexsetsize)  ){
    return(list("tree"=tree,"node"=node,"DATA"=data))
  }
  
  

  #Split (loops here)
  tree$childleft<- node
  lout<-create_tree(DATA,left_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  tree$left<-lout$tree
  tree$DATA<-lout$DATA
  node<-lout$node #pass the increased node to environment variable node


  tree$childright<-node
  rout<-create_tree(DATA,right_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  tree$right<-rout$tree
  tree$DATA<-rout$DATA 
  node<-rout$node

  
  
  
  tree$threshold<-sp$threshold
  tree$split_dir<-sp$split_dir
  tree$proj_data<-sp$proj_data
  
  
  # Add information on parent node
  tree$left$parentnode<- parentnode
  tree$right$parentnode<- parentnode
  

  # End
  return(list("tree"=tree,"node"=node,"DATA"=data))
}

