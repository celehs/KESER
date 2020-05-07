####### load data ########
load('data_now_use/data_embedding/data_example_depression.rda')
library(gglasso)
library(glmnet)
library(MASS)
library(openxlsx)

# Now I am printing each lambda and mse during training. 
# You could delete them.

loc.fit.RPDR <- loc.feature.selection(X_full_lst[[1]], Y_full_lst[[1]],
                                      X_train_lst[[1]], Y_train_lst[[1]], 
                                      X_valid_lst[[1]], Y_valid_lst[[1]],
                                      alpha = 0.5, lambda_lst = NULL, up_rate = 10, 
                                      drop_rate = 0.5, cos_cut = 0.1)

loc.fit.VA <- loc.feature.selection(X_full_lst[[2]], Y_full_lst[[2]],
                                    X_train_lst[[2]], Y_train_lst[[2]], 
                                    X_valid_lst[[2]], Y_valid_lst[[2]],
                                    alpha = 0.5, lambda_lst = NULL, up_rate = 10, 
                                    drop_rate = 0.5, cos_cut = 0.1)

int.fit.results <- int.feature.selection(X_full_lst[c(1,2)], Y_full_lst[c(1,2)], X_train_lst[c(1,2)],
                                         Y_train_lst[c(1,2)], X_valid_lst[c(1,2)], Y_valid_lst[c(1,2)],
                                         alpha = 0.5, lambda_single_lst = NULL, lambda_group_lst = NULL, 
                                         up.rate = 10, drop.rate = 0.5, cos.cut = 0.1)


which(int.fit.results$min.beta[[2]] != 0)
which(loc.fit.RPDR$min.beta != 0)
