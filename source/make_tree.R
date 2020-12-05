# Julio Change Detection
# make_tree.m
# 20201029 Xiaoyang Chen

#simulation settings
# setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
# data<-read.csv(file="Colon.txt", header=TRUE)
# cx <-as.matrix(data)
# mx <- dim(cx)[2]
# DATA<-cx


#start
`%notin%` <- Negate(`%in%`)

# nargin <- function() {
#   if(sys.nframe()<2) stop("must be called from inside a function")
#   length(as.list(sys.call(-1)))-1
# }

### function [tree,DATA] = make_tree(DATA,split_function,params)
make_tree<-function(DATA,split_function,params){
  # define nargin in R
  #print(match.call())
  nargin <- length(as.list(match.call())) -1

  ### define make tree function
  if (nargin<2){
    split_function='split_PCA'
    }
  #if (nargin<3){params<-matrix(NaN,1,mx)} # a row vector
  if (nargin<3){params<-data.frame(NaN)}
  if ('MAX_DEPTH' %notin% params){params$MAX_DEPTH=15}
  if ('split_fxn_params' %notin% params){   
    #spill <-0
    #split_fxn_params<-as.data.frame(spill)
    split_fxn_params<-data.frame(matrix(ncol = 1, nrow = 1)) #initialize null dataframe, TBD: get rid of this inirial NaN var
    split_fxn_params$spill<-0
    params$split_fxn_params<-split_fxn_params
    #str(params)
  }
  
  #Initialize first node
  node=0
  
  # Create tree
  #[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
  #ct=[tree, node,DATA]
  #idxs=1:nrow(DATA)
  #curr_depth=1
  # str(idxs) is a Integer Vector
  # idxs[1]
  ct<-create_tree(DATA,1:nrow(DATA),split_function,params$indexsetsize,params$split_fxn_params, params$MAX_DEPTH,1,node)
  ct$tree$size <- node
}

### define create_tree function
### [tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
create_tree<-function(DATA,idxs,split_function,indexsetsize,split_fxn_params, MAX_DEPTH,curr_depth,node){
  setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
  # path in package
  # source("split_KD.R")
  source(paste0(split_function,".R"))
  
  # initialize
  tree<-data.frame(matrix(ncol = 1, nrow = 1)) #initialize null dataframe
  tree$idxs = idxs
  mx <- dim(DATA)[2]
  tree$left = matrix(NaN,1,mx)
  tree$right = matrix(NaN,1,mx)
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
  parentnode = node
  
  # increase node
  node = node + 1
  #cellinfo <-data.frame(matrix(ncol = 1, nrow = 1))
  
  if ((curr_depth >= MAX_DEPTH) || (length(idxs)<= (ceiling(indexsetsize + 1 )))){
  # cellinfo[1] = tree
  # treelist = as.data.frame.matrix(table(treelist, cellinfo))
  #break--return value and exist the function;
  return(list(tree,node))
  }
  
  #[idx_left, idx_right, threshold, split_dir, proj_data] = split_function(DATA(idxs,:), split_fxn_params);
  # sp<- split_function(DATA[idxs,], split_fxn_params)
  split_function(DATA[idxs,], split_fxn_params)
  left_idxs <- idxs[idx_left]
  right_idxs <- idxs[idx_right]
  
  # Test
  if ((length(left_idxs) < indexsetsize) || (length(right_idxs) < indexsetsize)){
    return(list(tree,node))
  }
  
  # Split (loops here) # simulate by loop, check return
  tree$childleft<-node
  list(tree$left, node)<-create_tree(DATA,left_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  tree$childright<- node
  list(tree$right, node)<- create_tree(DATA,right_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  
  tree$threshold<-sp$threshold
  tree$split_dir<-sp$split_dir
  tree$proj_data<-sp$proj_data
  
  # Add information on parent node
  tree$left$parentnode<- parentnode
  tree$right$parentnode<- parentnode
  
  # return value
  #create_tree_result<-list(tree,node,DATA)
  return(list("tree"=tree,"node"=node))
}

