# Julio Change Detection
# Github split_KD.R
# 20201115 Xiaoyang Chen


# real data - 2000 gene site, 62 obs from tumor and normal tissue sample
# setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
# data<-as.matrix(read.csv(file="Colon.txt", header=TRUE))
# mx <- dim(data)[2]
# DATA<-t(data)

#function [left_idxs, right_idxs, threshold, split_dir, proj_data] = split_KD(DATA, params)
#                                                              split_function(DATA[idxs,], split_fxn_params)

split_KD<- function(DATA, params){

  `%notin%` <- Negate(`%in%`)
  if ('spill' %notin% params){params$spill=0}
  dir<-matrix(0,nrow=ncol(DATA),ncol=1)
  # str(dir)
  vrs<-apply(DATA,2,var) 
  v<-which.max(vrs)
  dir[v,1]<-1
  #return dimention of each col
  # https://www.mathworks.com/help/matlab/ref/var.html#bum85ue
  # testmatrix<-matrix(c(4,-2,1,9,5,7),nrow=2, ncol=3,byrow=TRUE)
  # testmatrix
  # vrst<-apply(testmatrix,2,var)
  # apply(testmatrix,2,var) == var(testmatrix,0,1)
  # vrst
  proj_data<-DATA%*%dir
  
  thresh<-median(proj_data)

  prc<-quantile(proj_data,c(0.5-params$spill, 0.5+params$spill))
  #prc<-quantile(A,c(0.25,0.75))
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

  # a<-list(left_idxs, right_idxs, threshold, split_dir, proj_data)
  # str(a)
  # names(a)
  # return(as.data.frame(left_idxs, right_idxs, threshold, split_dir, proj_data))
  return(list("left_idxs"=left_idxs, "right_idxs"=right_idxs, "threshold"=threshold, 
              "split_dir"=split_dir, "proj_data"=proj_data))

}





