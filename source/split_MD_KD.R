# Julio Change Detection
# Github split_MD_KD.R
# 20201020 Xiaoyang Chen
# Build tree

cx <-as.matrix(data) #data: 2000 gene site, 62 obs from tumor and normal tissue sample
dim(cx)
mx <- dim(cx)[2] 

dir<-matrix(0L,nrow=mx,ncol=1) # dir is a 0 matrix if 62*1
vrs<-apply(cx,2,var) # calculate variance of each tissue sample in 62 tissues
v<-max(vrs) # max variance

projdata<-cx%*%dir # but this projection is 0

#install.packages("mosaic")
#projdata<-project(cx,dir)

thresh=0.5
idx_left<-as.integer(projdata<=thresh)
idx_right<-as.integer(projdata>thresh)



