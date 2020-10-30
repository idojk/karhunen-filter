# Julio Change Detection
# make_tree.m
# 20201029 Xiaoyang Chen

# settings
cx <-as.matrix(data)
mx <- dim(cx)[2]
nargin=1

# start
if (nargin<2){split_function=split_PCA}
#if (nargin<3){params<-matrix(NaN,1,mx)} # a blank row vector
if (nargin<3){params<-data.frame(NaN)}
if ('MAX_DEPTH' %notin% params){params$MAX_DEPTH=15}
if ('split_fxn_params' %notin% params){   
  split_fxn_params$spill =0
  params$split_fxn_params =split_fxn_params
}

#Initialize first node
node=0

### Create tree
#[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
tree$size = node

#define create_tree function
node=0

#Create tree
#[tree, node,DATA] = create_tree(DATA,1:size(DATA,1),split_function,params.indexsetsize,params.split_fxn_params, params.MAX_DEPTH,1,node);
tree$size = node

# initialize
DATA<-cx
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


# Add depth to each node
tree$currentdepth = curr_depth - 1;

# Add node numbering
tree$node = node
parentnode = node

# increase node
node = node + 1
cellinfo = cell(1)

if (curr_depth >= MAX_DEPTH){
cellinfo[1] = tree
treelist = as.data.frame.matrix(table(treelist, cellinfo))
break
}
if (length(idxs)<= (ceil(indexsetsize + 1 ))){
cellinfo[1] = tree
treelist = as.data.frame.matrix(table(treelist, cellinfo))
break  
}

#[idx_left, idx_right, threshold, split_dir, proj_data] = split_function(DATA(idxs,:), split_fxn_params);
left_idxs = idxs[idx_left]
right_idxs = idxs[idx_right]
