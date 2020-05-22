####### load data ########
setwd('/Users/liumolei/Desktop/Research/cooccurance/')
source('function_for_ming.R')
load('data_now_use/data_embedding/data_example_depression.rda')
library(gglasso)
library(glmnet)
library(MASS)
library(openxlsx)

# Input cannot have columns with only 0 or just 1 single value. 
# So I add some code to delete the 0 columns, this needs not be used for depression.

"
for (t in 1:2) {
  X_f <- X_full_lst[[t]]
  X_t <- X_train_lst[[t]]
  use_set <- intersect(which(colMeans(X_f^2) > 1e-8), 
                       which(colMeans(X_t^2) > 1e-8))
  
  X_full_lst[[t]] <- X_f[,use_set]
  X_train_lst[[t]] <- X_t[,use_set]
  X_valid_lst[[t]] <- X_valid_lst[[t]][,use_set]

}
"

loc.fit.RPDR <- loc.feature.selection(X_full_lst[[1]], Y_full_lst[[1]],
                                      X_train_lst[[1]], Y_train_lst[[1]], 
                                      X_valid_lst[[1]], Y_valid_lst[[1]],
                                      alpha = 1, lambda_lst = NULL, up_rate = 10, 
                                      drop_rate = 0.5, cos_cut = 0.1, add.ridge = T)

loc.fit.VA <- loc.feature.selection(X_full_lst[[2]], Y_full_lst[[2]],
                                    X_train_lst[[2]], Y_train_lst[[2]], 
                                    X_valid_lst[[2]], Y_valid_lst[[2]],
                                    alpha = 1, lambda_lst = NULL, up_rate = 10, 
                                    drop_rate = 0.5, cos_cut = 0.1, add.ridge = T)


int.fit.results.fast <- int.feature.selection.fast(X_full_lst[c(1,2)], Y_full_lst[c(1,2)], X_train_lst[c(1,2)],
                                                   Y_train_lst[c(1,2)], X_valid_lst[c(1,2)], Y_valid_lst[c(1,2)],
                                                   lambda_lst = NULL, add.ridge = T)
int.fit.results.fast$min.beta

which(int.fit.results.fast$min.beta[[1]] != 0)
which(int.fit.results.fast$min.beta[[2]] != 0)


