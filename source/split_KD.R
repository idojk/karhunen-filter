# Julio Change Detection
# Github split_KD.R
# 20201115 Xiaoyang Chen


setwd("H:\\2021Sep\\Julio\\ChangeDetection\\ChangeDetectionR\\")
data<-read.csv(file="Colon.txt", header=TRUE)

cx <-as.matrix(data)
dim(cx)
mx <- dim(cx)[2] # mx is a dimention: 2000 gene site, 62 obs from tumor and normal tissue sample

#function [left_idxs, right_idxs, threshold, split_dir, proj_data] = split_KD(DATA, params)
if ('spill' %notin% params){params$spill=0}
dir<-matrix(0L,nrow=mx,ncol=1) # dir is a 0 matrix if n*1
vrs<-apply(cx,2,var)
v<-max(vrs)

projdata<-cx%*%dir

thresh<-median(projDATA,1);

prc = prctile(projDATA,[0.5-params.spill, 0.5+params.spill]*100);

comm_idx  = projDATA>prc(1) & projDATA<prc(2);
idx_left  = projDATA <= thresh;
idx_right = projDATA > thresh;

split_dir = dir;
threshold = thresh;
left_idxs = idx_left | comm_idx;
right_idxs = idx_right | comm_idx;
proj_data = projDATA;


