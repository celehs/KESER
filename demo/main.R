rm(list=ls())
setwd("/Users/chuanhong/Dropbox/00_Harvard/Share/Ming/")
library(data.table)
library(glmnet)
source("function.R")
load("dat.Rdata")

alpha=0.5
up.rate=10
drop.rate=0.5
method="lasso"
cos.cut=0.1
#method="dropout.with.y"
#method="dropout.without.y"
res=feature.sel.fun(x.A, y.A, x.sam, y.sam, x.sam2, y.sam2, method, alpha, nlambda=500, up.rate, drop.rate, cos.cut)
  
