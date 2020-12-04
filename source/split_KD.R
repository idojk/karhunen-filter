# Julio Change Detection
# Github split_KD.R
# 20201115 Xiaoyang Chen


#function [left_idxs, right_idxs, threshold, split_dir, proj_data] = split_KD(DATA, params)
split_KD<- function(DATA, params){

  `%notin%` <- Negate(`%in%`)
  if ('spill' %notin% params){params$spill=0}
  dir<-matrix(0L,nrow=mx,ncol=1) # dir is a 0 matrix if n*1
  vrs<-apply(cx,2,var)
  v<-max(vrs)
  
  proj_data<-cx%*%dir
  
  thresh<-median(proj_data)
  #proj_dataA<-c(1:100,1)
  prc<-quantile(proj_data,c(0.5-params$spill, 0.5+params$spill))
  #prc<-quantile(proj_data,c(0.25,0.75))
  comm_idx  = as.numeric(proj_data>prc[1] & proj_data<prc[2]) # T or F -> 1 or 0
  idx_left  = proj_data <= thresh #return T or F
  idx_right = proj_data > thresh
  
  split_dir<- dir
  threshold<- thresh
  left_idxs = as.numeric(idx_left | comm_idx) 
  right_idxs = as.numeric(idx_right | comm_idx)

  # a<-list(left_idxs, right_idxs, threshold, split_dir, proj_data)
  # str(a)
  # names(a)
  # return(as.data.frame(left_idxs, right_idxs, threshold, split_dir, proj_data))
  return(list("left_idxs"=left_idxs, "right_idxs"=right_idxs, "threshold"=threshold, 
              "split_dir"=split_dir, "proj_data"=proj_data))

}


