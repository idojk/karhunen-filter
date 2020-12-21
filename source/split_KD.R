# Julio Change Detection
# Github split_KD.R
# 20201115 Xiaoyang Chen
# complete date 12/20/2020


#function [left_idxs, right_idxs, threshold, split_dir, proj_data] = split_KD(DATA, params)
#                                                              split_function(DATA[idxs,], split_fxn_params)

split_KD<- function(DATA, params){
  
  `%notin%` <- Negate(`%in%`)
  if ('spill' %notin% params){params$spill=0}
  dir<-matrix(0,nrow=ncol(DATA),ncol=1)
  vrs<-apply(DATA,2,var) 
  

  v<-which.max(vrs)
  dir[v,1]<-1
  proj_data<-DATA%*%dir
  thresh<-median(proj_data)
  
  prc<-quantile(proj_data,c(0.5-params$spill, 0.5+params$spill))
  comm_idx  = as.numeric(proj_data>prc[1] & proj_data<prc[2])
  idx_left  = as.numeric(proj_data <= thresh) 
  idx_right = as.numeric(proj_data > thresh)
  # sum(comm_idx)
  # sum(idx_left)
  # sum(idx_right)
  
  split_dir<- dir
  threshold<- thresh
  left_idxs = as.numeric(idx_left | comm_idx) 
  right_idxs = as.numeric(idx_right | comm_idx)
  

  return(list("left_idxs"=left_idxs, "right_idxs"=right_idxs, "threshold"=threshold, 
              "split_dir"=split_dir, "proj_data"=proj_data))
  
}



