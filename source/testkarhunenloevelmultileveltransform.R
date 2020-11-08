# Julio Change Detection
# testkarhunenloevelmultileveltransform.m
# 20201107 Xiaoyang Chen

#settings
degree <-matrix(NaN,1,1)
numofpoints <-500

d <-1
data <-as.matrix(t(seq(0,d,len=numofpoints)))
n <-10
x <-as.matrix(seq(0,d,len=numofpoints))
M <-matrix(1,dim(x)[1],dim(x)[2])
Lc <-0.01
Lp <-max(d,2*Lc)
L  <-Lc / Lp


