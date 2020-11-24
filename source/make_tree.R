# Julio Change Detection
# make_tree.m
# 20201029 Xiaoyang Chen

#simulation settings
# setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
# data<-read.csv(file="Colon.txt", header=TRUE)
# cx <-as.matrix(data)
# mx <- dim(cx)[2]
# nargin=1
# DATA<-cx


#start
`%notin%` <- Negate(`%in%`)

### function [tree,DATA] = make_tree(DATA,split_function,params)
make_tree<-function(DATA,split_function,params){
  ### define make tree function
  if (nargin<2){
    split_function=split_PCA
    }
  #if (nargin<3){params<-matrix(NaN,1,mx)} # a row vector
  if (nargin<3){params<-data.frame(NaN)}
  if ('MAX_DEPTH' %notin% params){params$MAX_DEPTH=15}
  if ('split_fxn_params' %notin% params){   
    split_fxn_params$spill =0
    params$split_fxn_params=split_fxn_params
  }
  
  #Initialize first node
  node=0
  
  # Create tree
  #[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
  a<-create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node)
  tree$size = node
}

### define create_tree function
### [tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
create_tree<-function(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node)){
  setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
  # path in package
  source("split_KD.R")
  #source("split_function.R")
  
  # initialize
  idxs=10;
  tree$idxs = idxs
  tree$left = matrix(NaN,1,mx)
  tree$right = matrix(NaN,1,mx)
  tree$threshold = NaN
  tree$split_dir = NaN
  tree$proj_data =  matrix(NaN,1,mx)
  tree$center = mean(DATA[idxs,])
  tree$idxsmax = max(DATA[idxs,])
  tree$idxsmin = min(DATA[idxs,])
  
  # Add depth to each node: curr_depth not initialize in matlab code
  tree$currentdepth = curr_depth - 1;
  
  # Add node numbering
  tree$node = node
  parentnode = node
  
  # increase node
  node = node + 1
  cellinfo = cell(1)
  
  if (curr_depth >= MAX_DEPTH){
  # cellinfo[1] = tree
  # treelist = as.data.frame.matrix(table(treelist, cellinfo))
  break
  }
  if (length(idxs)<= (ceil(indexsetsize + 1 ))){
  # cellinfo[1] = tree
  # treelist = as.data.frame.matrix(table(treelist, cellinfo))
  break  
  }
  
  #[idx_left, idx_right, threshold, split_dir, proj_data] = split_function(DATA(idxs,:), split_fxn_params);
  b<- split_function(DATA(idxs,:), split_fxn_params)
  left_idxs = idxs[idx_left]
  right_idxs = idxs[idx_right]
  
  
  # Reorder nodes and data
  # size_left  = length(left_idxs);
  # size_right = length(right_idxs);
  # startnum = min(idxs);
  # endnum = max(idxs);
  # DATA(startnum : endnum, :) = [DATA(left_idxs,:); DATA(right_idxs,:)]; 
  # left_idxs = startnum : startnum + size_left - 1;
  # right_idxs = startnum + size_left : endnum;
  
  # Test
  if ((length(left_idxs) < indexsetsize) || (length(right_idxs) < indexsetsize)){break}
  
  # Split (loops here)
  tree$childleft<-node
  [tree$left, node]<-create_tree(DATA,left_idxs ,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  tree$childright<- node
  [tree$right, node]<- create_tree(DATA,right_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  
  tree$threshold<-threshold
  tree$split_dir<-split_dir
  tree$proj_data<-proj_data
  
  # Add information on parent node
  tree$left$parentnode<- parentnode
  tree$right$parentnode<- parentnode
}
