# Julio Change Detection
# make_tree.m
# 20201029 Xiaoyang Chen

#12/16 use matrix if list is slow, choose one
#12/16 timing compare to matlab
#validation: save R to .mat then run in matlab to see if they are the same

# setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
# real data
# data<-read.csv(file="Colon.txt", header=TRUE)

# simulate data
# cx <-as.matrix(data)
# mx <- dim(cx)[2]
# DATA<-t(cx)

#start
`%notin%` <- Negate(`%in%`)

### function [tree,DATA] = make_tree(DATA,split_function,params)
make_tree<-function(DATA,split_function,params){
  #define nargin in R
  #print(match.call())
  nargin <- length(as.list(match.call())) -1
  # nargin <- function() {
  #   if(sys.nframe()<2) stop("must be called from inside a function")
  #   length(as.list(sys.call(-1)))-1
  # }
  
  ### define make tree function
  if (nargin<2){
    split_function='split_PCA'
    }
  #if (nargin<3){params<-matrix(NaN,1,mx)} # a row vector
  if (nargin<3){params<-data.frame(NaN)}
  if ('MAX_DEPTH' %notin% params){params$MAX_DEPTH=15}
  if ('split_fxn_params' %notin% params){   
    spill <-0
    split_fxn_params<-as.data.frame(spill)
    # split_fxn_params<-data.frame(matrix(ncol = 1, nrow = 1))
    # split_fxn_params$spill<-0
    params$split_fxn_params<-split_fxn_params
    #str(params)
  }
  
  #Initialize first node
  node=0
  
  # Create tree
  #[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
  ct<-create_tree(DATA,1:nrow(DATA),split_function,params$indexsetsize,params$split_fxn_params, params$MAX_DEPTH,1,node)
  ct$tree$size <- node
}


### define create_tree function
# Input from last function
# str(DATA[idxs,])
# str(DATA)

# DATA<-as.matrix(seq(0,d,len=numofpoints))
# curr_depth=1
# idxs=1:nrow(DATA)
# idxs[1]
# MAX_DEPTH<-params$MAX_DEPTH
# params$MAX_DEPTH=15
# spill <-0
# split_fxn_params<-as.data.frame(spill)
# params$split_fxn_params<-split_fxn_params
# node=0

### [tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
create_tree<-function(DATA,idxs,split_function,indexsetsize,split_fxn_params, MAX_DEPTH,curr_depth,node){
  setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\") #change to package path

  # initialize
  tree<-data.frame(matrix(NaN,ncol = 1, nrow = 1))  #initialize null dataframe
  #tree$idxs <-t(as.matrix(idxs))
  tree$idxs <-list(idxs)
  mx <- dim(DATA)[1]
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
  
  if (isTRUE(curr_depth>=MAX_DEPTH)) {
    return(data.frame(tree,node,data))
    }
  if (isTRUE( length(idxs)<=ceiling(indexsetsize + 1) )) {
    return(data.frame(tree,node,data))
    }

  
  
  #[idx_left, idx_right, threshold, split_dir, proj_data] = split_function(DATA(idxs,:), split_fxn_params);
  source('split_KD.R')
  # source(paste('"',split_function,'.R"'))
  # paste0('"',split_function,'.R"')
  # gsub(" ",paste("'",split_function,".R'"))
  # source(paste0("'",split_function,".R'"))
  # split_function=split_KD 
  # split_function2='split_KD'
  # str(split_function)
  # str(split_function2)
  # print(str(split_function))
  # source(paste0("'",split_function,"'.R")) # instead of split_function='split_KD'
  
  #   cannot coerce type 'closure' to vector of type 'character'
 

  sp<-split_KD(DATA, split_fxn_params)
  # split_function(DATA[idxs,], split_fxn_params)
  # split_KD(DATA[idxs,], split_fxn_params)
  # str(sp)
  
  #overwrite left_idxs and right_idxs for next layer
  left_idxs <- idxs*sp$left_idx
  left_idxs<-left_idxs[left_idxs!=0]
  right_idxs <- idxs*sp$right_idxs
  right_idxs<-right_idxs[right_idxs!=0]
  
  # Test
  if (  (length(left_idxs)< indexsetsize) || (length(right_idxs) < indexsetsize)  ){
    return(data.frame(tree,node,data))
  }
  
  
  # Split (loops here) 
  tree$childleft<-node
  tree$left<-create_tree(DATA,left_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  tree$childright<- node
  tree$right<-create_tree(DATA,right_idxs,split_function,indexsetsize,split_fxn_params,MAX_DEPTH,curr_depth+1,node)
  
  tree$threshold<-sp$threshold
  tree$split_dir<-sp$split_dir
  tree$proj_data<-sp$proj_data
  
  # Add information on parent node
  tree$left$parentnode<- parentnode
  tree$right$parentnode<- parentnode
 
  rtree<-data.frame(tree,node,data)
  # rtree<-data.frame(t(c(tree,node,data)))
  # return value: should use data.frame
  return(rtree)
  # return(list("tree"=tree,"node"=node,"DATA"=data))
}

# tree$left$left$left shou be the child level
# find equv of cells in R
# list after multilevel part
