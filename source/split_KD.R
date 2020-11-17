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

projDATA<-cx%*%dir
thresh<-median(projDATA)
  
prc<-quantile(projDATA,c((0.5-params$spill)*100, (0.5+params$spill)*100))
comm_idx  = projDATA>prc[1] & projDATA<prc[2] #returnT or F
idx_left  = projDATA <= thresh #returnT or F
idx_right = projDATA > thresh

split_dir<- dir
threshold<- thresh
left_idxs = idx_left | comm_idx #return T or F
right_idxs = idx_right | comm_idx
proj_data <- projDATA

split_KD<-list(left_idxs, right_idxs, threshold, split_dir, proj_data)
return(split_KD)
}


