# Used for adding ridge penalty to the covariates
add_ridge <- function(X_1, Y_1, lambda, alpha = 0.5){
  n <- nrow(X_1)
  p <- ncol(X_1)
  Cov_all <- t(cbind(X_1, Y_1)) %*% cbind(X_1, Y_1)
  Cov_all <- diag(lambda * alpha * c(rep(1, p), 0)) + Cov_all
  svd_result <- svd(Cov_all)
  s_value <- svd_result$d
  s_mat <- diag(sqrt(s_value))
  data_all <- svd_result$u %*% s_mat
  X <- t(data_all[-length(data_all[ ,1]),])
  Y <- data_all[length(data_all[ ,1]),]
  return(list(X = X, Y = Y))
}

# Fit local lasso with the validation set to tune
Lasso_select_drop <- function(X, Y, X_valid, Y_valid, X_all, Y_all, lambda_lst,
                              alpha = 0.5){
  min.lambda <- NULL
  min.coef <- NULL
  min.mse <- Inf
  
  fit_result <- glmnet::glmnet(X, Y, lambda = lambda_lst, intercept = F, alpha = alpha,
                               standardize = F)
  
  for (l in 1:length(lambda_lst)) {
    lambda <- fit_result$lambda[l]
    fit_coef <- as.vector(fit_result$beta[,l])
    print(length(which(fit_coef != 0)))
    mse <- mean((Y_valid - as.matrix(X_valid) %*% fit_coef)^2)
    if (mse < min.mse){
      min.lambda <- lambda
      min.mse <- mse
      min.coef <- fit_coef
    }
    print(mse)
  }
  
  fit_result_all <- glmnet::glmnet(X_all, Y_all, lambda = min.lambda, intercept = F,
                                   alpha = alpha, standardize = F)
  min.coef <- as.vector(fit_result_all$beta)
  
  return(list(min.lambda = min.lambda, min.coef = min.coef, min.mse = min.mse))
}


# Fit integrative group lasso with the validation set to tune

Combine_fit_drop <- function(X_lst, Y_lst, X_lst_valid, Y_lst_valid, common_indx, 
                             lambda_M_lst, lambda_coef_lst, lambda_g_lst, alpha = 0.5){
  M <- length(Y_lst)
  options(warn = -1)
  
  p_lst <- c()
  for (m in 1:M) {
    p_lst <- c(p_lst, length(X_lst[[m]][1, ]))
  }
  
  ############# Creat group index #############
  
  indx_lst <- c(1:length(X_lst[[1]][1, ]))
  
  for (m in 2:M){ 
    indx_m <- 1:(p_lst[m]) + sum(p_lst[1:(m - 1)])
    indx_m[common_indx[[m]]] <- c(1:p_lst[1])[common_indx[[1]]]
    indx_lst <- c(indx_lst, indx_m)
  }
  
  for (t in 2:length(indx_lst)){
    if (length(which(indx_lst == indx_lst[t])) == 1){
      indx_lst[t] <- max(indx_lst[1:(t - 1)]) + 1
    }
  }
  
  ############# Add ridge penalty and bind X together for as a matrix #############
  
  X_1 <- X_lst[[1]]
  Y_1 <- Y_lst[[1]]
  
  data_ridge <- add_ridge(X_1, Y_1, lambda_M_lst[1], alpha = alpha)
  
  X_all <- data_ridge$X
  Y_all <- data_ridge$Y
  X_all_valid <- X_lst_valid[[1]]
  Y_all_valid <- Y_lst_valid[[1]]
  
  for (m in 2:M){
    X_m <- X_lst[[m]]
    Y_m <- Y_lst[[m]]
    data_ridge <- add_ridge(X_m, Y_m, lambda_M_lst[m], alpha = alpha)
    X_all <- Matrix::bdiag(X_all, data_ridge$X)
    Y_all <- c(Y_all, data_ridge$Y)
    X_all_valid <- Matrix::bdiag(X_all_valid, X_lst_valid[[m]])
    Y_all_valid <- c(Y_all_valid, Y_lst_valid[[m]])
  }
  
  lambda_M_lst <- lambda_M_lst / nrow(X_all)
  lambda <- mean(lambda_M_lst)
  group_id <- 1:length(unique(indx_lst))
  
  # tune with SSE
  min.lambda <- NULL
  min.coef <- NULL
  min.sse <- Inf
  
  penalty_factor <- c()
  group_ind_vec <- c()
  indx_convert <- c()
  
  ############# Creat group index and penalty factor to fit for gglasso() #############
  
  for (t in 1:length(group_id)){
    if (length(which(indx_lst == group_id[t])) == 2){
      penalty_factor <- c(penalty_factor, 3)
      group_ind_vec <- c(group_ind_vec, c(group_id[t], group_id[t]))
    }else{
      if (group_id[t] <= p_lst[1]){
        penalty_factor <- c(penalty_factor, 1)
      }else{
        penalty_factor <- c(penalty_factor, 2)
      }
      group_ind_vec <- c(group_ind_vec, group_id[t])
    }
    indx_convert <- c(indx_convert, which(indx_lst == group_id[t]))
  }
  X_all <- X_all[,indx_convert]
  X_all_valid <- X_all_valid[,indx_convert]
  
  for (lambda_coef in lambda_coef_lst) {
    for (lambda_g in lambda_g_lst){
      
      penalty_fac_lam <- penalty_factor
      penalty_fac_lam[which(penalty_fac_lam == 1)] <- lambda_M_lst[1] / lambda
      penalty_fac_lam[which(penalty_fac_lam == 2)] <- lambda_M_lst[2] / lambda
      penalty_fac_lam[which(penalty_fac_lam == 3)] <- lambda_g
      penalty_fac_lam <- lambda_coef * penalty_fac_lam
      
      fit_result <- gglasso::gglasso(x = as.matrix(X_all), y = Y_all, group = group_ind_vec, 
                                     lambda = lambda, pf = penalty_fac_lam, intercept = F)
      fit_coef <- fit_result$beta
      
      sse <- sum((Y_all_valid - X_all_valid %*% fit_coef)^2)
      print(sse)
      print(length(which(fit_coef != 0)))
      
      if (sse < min.sse){
        min.lambda <- c(lambda_coef, lambda_g)
        min.sse <- sse
        min.coef <- fit_coef
      }
      
    }
  }
  
  ############# bind back to form beta according to the original index #############
  
  coef_final <- rep(NA, length(min.coef))
  for (k in 1:length(indx_convert)) {
    coef_final[indx_convert[k]] <- min.coef[k]
  }
  
  beta_lst <- vector('list', M)
  pos.cut <- 1
  for (m in 1:M) {
    pos.end <- pos.cut + p_lst[m] - 1
    beta_lst[[m]] <- coef_final[pos.cut:pos.end]
    pos.cut <- pos.end + 1
  }
  
  return(list(min.lambda = min.lambda, min.beta = beta_lst, min.sse = sse))
}


# Main function for integrative feature selection

int.feature.selection <- function(X_full_lst, Y_full_lst, X_train_lst,
                                  Y_train_lst, X_valid_lst, Y_valid_lst,
                                  alpha = 0.5, lambda_single_lst = NULL, lambda_group_lst = NULL, 
                                  up.rate = 10, drop.rate = 0.5, cos.cut = 0.1){
  
  ########## Find the set of covariates shared by the two studies ##########
  
  common_set <- intersect(colnames(X_full_lst[[1]]), colnames(X_full_lst[[2]]))
  common_indx <- vector('list', 2)
  for (variable in common_set) {
    common_indx[[1]] <- c(common_indx[[1]], which(colnames(X_full_lst[[1]]) == variable))
    common_indx[[2]] <- c(common_indx[[2]], which(colnames(X_full_lst[[2]]) == variable))
  }
  
  
  ######### dropout and up-sample ############
  
  p1 <- length(X_full_lst[[1]][1,])
  p2 <- length(X_full_lst[[2]][1,])
  n1 <- length(X_full_lst[[1]][,1])
  n2 <- length(X_full_lst[[2]][,1])
  
  for (m in 1:2) {
    drop_m_full <- drop_fun(X_full_lst[[m]], Y_full_lst[[m]], drop_rate = 0.5, up_rate = 10)
    X_full_lst[[m]] <- drop_m_full$X
    Y_full_lst[[m]] <- drop_m_full$Y
    drop_m_train <- drop_fun(X_train_lst[[m]], Y_train_lst[[m]], drop_rate = 0.5, up_rate = 10)
    X_train_lst[[m]] <- drop_m_train$X
    Y_train_lst[[m]] <- drop_m_train$Y
  }
  
  ######### Fit two local regression to specify the proportion of lambda ###########
  ######### Aim of this is to safe the time from tuning too many parameters ########
  
  fit.local1 <- Lasso_select_drop(X_train_lst[[1]], Y_train_lst[[1]], X_valid_lst[[1]], 
                                  Y_valid_lst[[1]], X_full_lst[[1]], Y_full_lst[[1]],
                                  lambda_lst = 0.3 * c(c(1:400) * 0.0003, c(41:400) * 0.003, c(41:1500) * 0.03) *
                                    sd(Y_train_lst[[1]]) * sqrt(log(p1) / n1), alpha = alpha)
  fit.local2 <- Lasso_select_drop(X_train_lst[[2]], Y_train_lst[[2]], X_valid_lst[[2]],
                                  Y_valid_lst[[2]], X_full_lst[[2]], Y_full_lst[[2]], 
                                  lambda_lst = 0.4 * c(c(1:400) * 0.001, c(81:200) * 0.005, c(51:1800) * 0.02) *
                                    sd(Y_train_lst[[2]]) * sqrt(log(p2) / n2), alpha = alpha)
  if (is.null(lambda_single_lst)){
    lambda_single_lst <- 0.0005 * c(1:200)
  }
  if (is.null(lambda_group_lst)){
    lambda_group_lst <- 0.75 * c(c(5:25) * 0.02, c(7:300) * 0.08)
  }
  
  ######### Tuning parameter ###########
  
  fit.grp <- Combine_fit_drop(X_train_lst, Y_train_lst, X_valid_lst, Y_valid_lst, common_indx, 
                              lambda_M_lst = 4 / (sd(Y_train_lst[[1]]) + sd(Y_train_lst[[2]])) * 
                                c(length(Y_train_lst[[1]]) * fit.local1$min.lambda, 
                                  length(Y_train_lst[[2]]) * fit.local2$min.lambda),
                              lambda_coef_lst = lambda_single_lst,
                              lambda_g_lst = lambda_group_lst)
  
  ######### Train with the tuned parameters ###########
  
  fit.grp.full <- Combine_fit_drop(X_full_lst, Y_full_lst, X_valid_lst, Y_valid_lst, common_indx, 
                                   lambda_M_lst = 4 / (sd(Y_train_lst[[1]]) + sd(Y_train_lst[[2]])) * 
                                     c(length(Y_train_lst[[1]]) * fit.local1$min.lambda, 
                                       length(Y_train_lst[[2]]) * fit.local2$min.lambda),
                                   lambda_coef_lst = fit.grp$min.lambda[1],
                                   lambda_g_lst = fit.grp$min.lambda[2])
  
  beta_lst <- fit.grp.full$min.beta
  
  for (m in 1:2){
    beta_lst[[m]] <- as.data.frame(beta_lst[[m]])
    rownames(beta_lst[[m]]) <- colnames(X_full_lst[[m]])
  }
  return(list(min.lambda = fit.grp$min.lambda, min.beta = beta_lst))
  
}
