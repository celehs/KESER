rm(list=ls())
library(grplasso)
library(gglasso)
library(glmnet)
library(MASS)
library(dplyr)
setwd('/Users/chuanhong/Dropbox/00_Harvard/Feature_knowledge/revision/tuning/')
setwd('~/Dropbox/Collaborative/Phenotyping/VA_P30/Manuscript/Feature_knowledge/revision/tuning/')

setwd('/Users/liumolei/111 Dropbox/Molei Liu/Feature_knowledge/revision/tuning/')


source("library_v2.R")
source("function_for_ming_01092020.R")

add.ridge=TRUE
ridge.tuning='MSE'
up_rate=1
drop_rate=0
yes.stand=1 ## using SD to standardize
yes.weight=1 ## using cosine as weight
method='ppmi'
d0=9999 ## optimal dimension

load("dict_combine_uniform_molei.Rdata")
dict.combine=dict.combine[,c("feature_id", "feature_desc")]
colnames(dict.combine)=c("Variable","Description")

load("dat-PheCode:714.1-ppmi-d9999.Rdata")
cos.RPDR=data.frame(Variable=names(cos_full_lst[[1]]), cos=cos_full_lst[[1]])
cos.VA=data.frame(Variable=names(cos_full_lst[[2]]), cos=cos_full_lst[[2]])

set.seed(1234)
loc.fit.RPDR <- loc.feature.selection.new(X_full_local_lst[[1]], Y_full_lst[[1]],
                                          X_train_local_lst[[1]], Y_train_lst[[1]], 
                                          X_valid_local_lst[[1]], Y_valid_lst[[1]],
                                          alpha = 1, lambda_lst = NULL, up_rate = up_rate, 
                                          drop_rate = drop_rate,  add.ridge = add.ridge, ridge.tuning=ridge.tuning,yes.stand=yes.stand, yes.weight=yes.weight)
beta.RPDR=data.frame(Variable=rownames(loc.fit.RPDR$min.beta), beta=loc.fit.RPDR$min.beta)
tab.RPDR=left_join(beta.RPDR, cos.RPDR, by="Variable")
tab.RPDR=left_join(tab.RPDR, dict.combine, by="Variable")
print(sum(tab.RPDR$beta_loc!=0))


loc.fit.RPDR$min.beta[rownames(loc.fit.RPDR$min.beta)=="RXNORM:8640", ]
loc.fit.RPDR$min.beta[rownames(loc.fit.RPDR$min.beta)=="RXNORM:4511", ]
loc.fit.RPDR$min.beta[rownames(loc.fit.RPDR$min.beta)=="RXNORM:6902", ]

loc.fit.VA <- loc.feature.selection.new(X_full_local_lst[[2]], Y_full_lst[[2]],
                                        X_train_local_lst[[2]], Y_train_lst[[2]], 
                                        X_valid_local_lst[[2]], Y_valid_lst[[2]],
                                        alpha = 1, lambda_lst = NULL, up_rate = up_rate, 
                                        drop_rate = drop_rate, add.ridge = add.ridge, ridge.tuning=ridge.tuning, yes.stand=yes.stand, yes.weight=yes.weight)
beta.VA=data.frame(Variable=rownames(loc.fit.VA$min.beta), beta=loc.fit.VA$min.beta)
tab.VA=left_join(beta.VA, cos.VA, by="Variable")
tab.VA=left_join(tab.VA, dict.combine, by="Variable")
print(sum(tab.VA$beta_loc!=0))

loc.fit.VA$min.beta[rownames(loc.fit.VA$min.beta)=="RXNORM:8640", ]
loc.fit.VA$min.beta[rownames(loc.fit.VA$min.beta)=="RXNORM:4511", ]
loc.fit.VA$min.beta[rownames(loc.fit.VA$min.beta)=="RXNORM:6902", ]

########### Run integrative regression ########### 
int.fit.results.fast <- int.feature.selection.fast.new(X_full_lst[c(1,2)], Y_full_lst[c(1,2)], X_train_lst[c(1,2)],
                                                       Y_train_lst[c(1,2)], X_valid_lst[c(1,2)], Y_valid_lst[c(1,2)],
                                                       drop_rate = drop_rate, ridge.tuning=ridge.tuning, 
                                                       add.ridge = add.ridge, up_rate = up_rate, yes.stand=yes.stand,
                                                       yes.weight = yes.weight)

length(which(int.fit.results.fast$min.beta$Study_1 != 0))
length(which(int.fit.results.fast$min.beta$Study_2 != 0))

int.fit.results.fast$min.beta$Study_1[which(rownames(int.fit.results.fast$min.beta) == "RXNORM:8640")]
int.fit.results.fast$min.beta$Study_2[which(rownames(int.fit.results.fast$min.beta) == "RXNORM:8640")]

int.fit.results.fast$min.beta$Study_1[which(rownames(int.fit.results.fast$min.beta) == "RXNORM:4511")]
int.fit.results.fast$min.beta$Study_2[which(rownames(int.fit.results.fast$min.beta) == "RXNORM:4511")]

int.fit.results.fast$min.beta$Study_1[which(rownames(int.fit.results.fast$min.beta) == "RXNORM:6902")]
int.fit.results.fast$min.beta$Study_2[which(rownames(int.fit.results.fast$min.beta) == "RXNORM:6902")]

