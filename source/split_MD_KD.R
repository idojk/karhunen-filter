# Julio Change Detection
# Github snapshots.R
# 20201012 Xiaoyang Chen
# Build tree

cx <- data
mx <- dim(cx)[2] # mx is a dimention: 62 tumor cells

dir<-matrix(0L,nrow=mx,ncol=1) # dir is a 0 matrix
vrs<-apply(cx,2,var) # calculate variance of each tumor cell
v<-max(vrs) # max variance
mx
dir

projdata=data*dir # but this projection is 0
thresh=0.5

if (projDATA<= thresh) {
  idx_left<-projDATA
  }   
else {
  idx_right<-projDATA
  }



