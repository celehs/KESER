library(MASS)
library(glmnet)
library(gglasso)

loc.fit.RPDR <- loc.feature.selection(
  data$X_full_lst[[1]], 
  data$Y_full_lst[[1]],
  data$X_train_lst[[1]], 
  data$Y_train_lst[[1]], 
  data$X_valid_lst[[1]], 
  data$Y_valid_lst[[1]],
  alpha = 0.5, 
  lambda_lst = NULL, 
  up_rate = 10, 
  drop_rate = 0.5, 
  cos_cut = 0.1)

loc.fit.VA <- loc.feature.selection(
  data$X_full_lst[[2]], 
  data$Y_full_lst[[2]],
  data$X_train_lst[[2]], 
  data$Y_train_lst[[2]], 
  data$X_valid_lst[[2]], 
  data$Y_valid_lst[[2]],
  alpha = 0.5, 
  lambda_lst = NULL, 
  up_rate = 10, 
  drop_rate = 0.5, 
  cos_cut = 0.1)

which(loc.fit.RPDR$min.beta != 0)
# which(loc.fit.VA$min.beta != 0)
