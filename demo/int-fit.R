library(MASS)
library(glmnet)
library(gglasso)

data <- readRDS("rdata/data.rds")

int.fit.results <- int.feature.selection(
  data$X_full_lst[c(1, 2)], 
  data$Y_full_lst[c(1, 2)], 
  data$X_train_lst[c(1, 2)],
  data$Y_train_lst[c(1, 2)], 
  data$X_valid_lst[c(1, 2)], 
  data$Y_valid_lst[c(1, 2)],
  alpha = 0.5, 
  lambda_single_lst = NULL, 
  lambda_group_lst = NULL, 
  up.rate = 10, 
  drop.rate = 0.5, 
  cos.cut = 0.1)

which(loc.fit.RPDR$min.beta != 0)
