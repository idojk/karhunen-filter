# Julio Change Detection
# make_tree.m
# 20201029 Xiaoyang Chen

nargin=1
if (nargin<2){split_function=split_PCA}
#if (nargin<3){params<-matrix(NaN,1,cx)} # a blank row vector
if (nargin<3){params<-data.frame(NaN)}
if ('MAX_DEPTH' %notin% params){params$MAX_DEPTH=15}
if ('split_fxn_params' %notin% params){   
  split_fxn_params$spill =0
  params$split_fxn_params =split_fxn_params
}

#Initialize first node
node=0

#Create tree
#[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
tree$size = node

#define create_tree function
